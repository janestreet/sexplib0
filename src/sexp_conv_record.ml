open! StdLabels
open Basement
open Sexp_conv
open Sexp_conv_error

module Kind = struct
  type (_, _) t =
    | Default : ('a : any). (unit -> 'a) -> (unit -> 'a, Sexp.t -> unit -> 'a) t
    | Omit_nil : ('a : any). (unit -> 'a, Sexp.t -> unit -> 'a) t
    | Required : ('a : any). (unit -> 'a, Sexp.t -> unit -> 'a) t
    | Sexp_array : ('a array, Sexp.t -> 'a) t
    | Sexp_bool : (bool, unit) t
    | Sexp_list : ('a list, Sexp.t -> 'a) t
    | Sexp_option : ('a option, Sexp.t -> 'a) t
end

module Fields = struct
  type _ t =
    | Empty : unit t
    | Field :
        { name : string
        ; kind : ('a, 'conv) Kind.t
        ; conv : 'conv
        ; rest : 'b t
        }
        -> ('a * 'b) t

  let length =
    let rec length_loop : type a. a t -> int -> int =
      fun t acc ->
      match t with
      | Field { rest; _ } -> length_loop rest (acc + 1)
      | Empty -> acc
    in
    fun t -> length_loop t 0
  ;;
end

module Malformed = struct
  (* Represents errors that can occur due to malformed record sexps. Accumulated as a
     value so we can report multiple names at once for extra fields, duplicate fields, or
     missing fields. *)
  type t =
    | Bool_payload
    | Missing_and_extras of
        { missing : string list
        ; extras : string list
        }
    | Dups of string list
    | Non_pair of Sexp.t option

  let missing missing = Missing_and_extras { missing; extras = [] }
  let extras extras = Missing_and_extras { missing = []; extras }

  let combine a b =
    match a, b with
    (* choose the first bool-payload or non-pair error that occurs *)
    | ((Bool_payload | Non_pair _) as t), _ -> t
    | _, ((Bool_payload | Non_pair _) as t) -> t
    (* combine lists of similar errors *)
    | ( Missing_and_extras { missing = missing_a; extras = extras_a }
      , Missing_and_extras { missing = missing_b; extras = extras_b } ) ->
      Missing_and_extras { missing = missing_a @ missing_b; extras = extras_a @ extras_b }
    | Dups a, Dups b -> Dups (a @ b)
    (* otherwise, dups > extras > missing *)
    | (Dups _ as t), _ | _, (Dups _ as t) -> t
  ;;

  let raise t ~caller ~context =
    match t with
    | Bool_payload -> record_sexp_bool_with_payload caller context
    | Missing_and_extras { missing; extras } ->
      record_missing_and_extra_fields caller ~missing ~extras context
    | Dups names -> record_duplicate_fields caller names context
    | Non_pair maybe_context ->
      let context = Option.value maybe_context ~default:context in
      record_only_pairs_expected caller context
  ;;
end

exception Malformed of Malformed.t

module State = struct
  (* Stores sexps corresponding to record fields, in the order the fields were declared.
     Excludes fields already parsed in the fast path.

     List sexps represent a field that is present, such as (x 1) for a field named "x".
     Atom sexps represent a field that is absent, or at least not yet seen. *)
  type t = { state : Sexp.t array } [@@unboxed]

  let unsafe_get t pos = Array.unsafe_get t.state pos
  let unsafe_set t pos sexp = Array.unsafe_set t.state pos sexp
  let create len = { state = Array.make len (Sexp.Atom "") }
end

(* Parsing field values from state. *)

let rec parse_value_malformed
  : type a b. Malformed.t -> fields:(a * b) Fields.t -> state:State.t -> pos:int -> a
  =
  fun malformed ~fields ~state ~pos ->
  let (Field field) = fields in
  let malformed =
    match parse_values ~fields:field.rest ~state ~pos:(pos + 1) with
    | (_ : b) -> malformed
    | exception Malformed other -> Malformed.combine malformed other
  in
  raise (Malformed malformed)

and[@tail_mod_cons] parse_value
  : type a b. fields:(a * b) Fields.t -> state:State.t -> pos:int -> a * b
  =
  fun ~fields ~state ~pos ->
  let (Field { name; kind; conv; rest }) = fields in
  let value : a =
    match kind, State.unsafe_get state pos with
    (* well-formed *)
    | Required, List [ _; sexp ] -> conv sexp
    | Default _, List [ _; sexp ] -> conv sexp
    | Omit_nil, List [ _; sexp ] -> conv sexp
    | Sexp_option, List [ _; sexp ] -> Some (conv sexp)
    | Sexp_list, List [ _; sexp ] -> list_of_sexp conv sexp
    | Sexp_array, List [ _; sexp ] -> array_of_sexp conv sexp
    | Sexp_bool, List [ _ ] -> true
    (* ill-formed *)
    | ( (Required | Default _ | Omit_nil | Sexp_option | Sexp_list | Sexp_array)
      , (List (_ :: _ :: _ :: _) as sexp) ) ->
      parse_value_malformed (Non_pair (Some sexp)) ~fields ~state ~pos
    | ( (Required | Default _ | Omit_nil | Sexp_option | Sexp_list | Sexp_array)
      , List ([] | [ _ ]) ) -> parse_value_malformed (Non_pair None) ~fields ~state ~pos
    | Sexp_bool, List ([] | _ :: _ :: _) ->
      parse_value_malformed Bool_payload ~fields ~state ~pos
    (* absent *)
    | Required, Atom _ ->
      parse_value_malformed (Malformed.missing [ name ]) ~fields ~state ~pos
    | Default default, Atom _ -> default
    | Omit_nil, Atom _ -> conv (List [])
    | Sexp_option, Atom _ -> None
    | Sexp_list, Atom _ -> []
    | Sexp_array, Atom _ -> [||]
    | Sexp_bool, Atom _ -> false
  in
  value, parse_values ~fields:rest ~state ~pos:(pos + 1)

and[@tail_mod_cons] parse_values
  : type a. fields:a Fields.t -> state:State.t -> pos:int -> a
  =
  fun ~fields ~state ~pos ->
  match fields with
  | Field _ -> parse_value ~fields ~state ~pos
  | Empty -> ()
;;

(* Populating state. Handles slow path cases where there may be reordered, duplicated,
   missing, or extra fields. *)

let rec parse_spine_malformed malformed ~index ~extra ~seen ~state ~len sexps =
  let malformed =
    match parse_spine_slow ~index ~extra ~seen ~state ~len sexps with
    | () -> malformed
    | exception Malformed other -> Malformed.combine malformed other
  in
  raise (Malformed malformed)

and parse_spine_slow ~index ~extra ~seen ~state ~len sexps =
  match (sexps : Sexp.t list) with
  | [] -> ()
  | (List (Atom name :: _) as field) :: sexps ->
    let i = index name in
    (match seen <= i && i < len with
     | true ->
       (* valid field for slow-path parsing *)
       let pos = i - seen in
       (match State.unsafe_get state pos with
        | Atom _ ->
          (* field not seen yet *)
          State.unsafe_set state pos field;
          parse_spine_slow ~index ~extra ~seen ~state ~len sexps
        | List _ ->
          (* field already seen *)
          parse_spine_malformed (Dups [ name ]) ~index ~extra ~seen ~state ~len sexps)
     | false ->
       (match 0 <= i && i < seen with
        | true ->
          (* field seen in fast path *)
          parse_spine_malformed (Dups [ name ]) ~index ~extra ~seen ~state ~len sexps
        | false ->
          (* extra field *)
          (match extra with
           | true -> parse_spine_slow ~index ~extra ~seen ~state ~len sexps
           | false ->
             parse_spine_malformed
               (Malformed.extras [ name ])
               ~index
               ~extra
               ~seen
               ~state
               ~len
               sexps)))
  | sexp :: sexps ->
    parse_spine_malformed (Non_pair (Some sexp)) ~index ~extra ~seen ~state ~len sexps
;;

(* Slow path for record parsing. Uses state to store fields as they are discovered. *)

let parse_record_slow ~fields ~index ~extra ~seen sexps =
  let unseen = Fields.length fields in
  let state = State.create unseen in
  let len = seen + unseen in
  (* populate state *)
  let maybe_malformed =
    match parse_spine_slow ~index ~extra ~seen ~state ~len sexps with
    | exception Malformed malformed -> Some malformed
    | () -> None
  in
  (* parse values from state *)
  let parsed_or_malformed =
    match parse_values ~fields ~state ~pos:0 with
    | values -> Ok values
    | exception Malformed malformed -> Error malformed
  in
  match maybe_malformed, parsed_or_malformed with
  | None, Ok values -> values
  | Some malformed, Ok _ | None, Error malformed -> raise (Malformed malformed)
  | Some malformed1, Error malformed2 ->
    raise (Malformed (Malformed.combine malformed1 malformed2))
;;

(* Fast path for record parsing. Directly parses and returns fields in the order they are
   declared. Falls back on slow path if any fields are absent, reordered, or malformed. *)

let[@tail_mod_cons] rec parse_field_fast
  : type a b.
    fields:(a * b) Fields.t
    -> index:(string -> int)
    -> extra:bool
    -> seen:int
    -> Sexp.t list
    -> a * b
  =
  fun ~fields ~index ~extra ~seen sexps ->
  let (Field { name; kind; conv; rest }) = fields in
  match sexps with
  | List (Atom atom :: args) :: others when String.equal atom name ->
    (match kind, args with
     | Required, [ sexp ] ->
       conv sexp, parse_spine_fast ~fields:rest ~index ~extra ~seen:(seen + 1) others
     | Default _, [ sexp ] ->
       conv sexp, parse_spine_fast ~fields:rest ~index ~extra ~seen:(seen + 1) others
     | Omit_nil, [ sexp ] ->
       conv sexp, parse_spine_fast ~fields:rest ~index ~extra ~seen:(seen + 1) others
     | Sexp_option, [ sexp ] ->
       ( Some (conv sexp)
       , parse_spine_fast ~fields:rest ~index ~extra ~seen:(seen + 1) others )
     | Sexp_list, [ sexp ] ->
       ( list_of_sexp conv sexp
       , parse_spine_fast ~fields:rest ~index ~extra ~seen:(seen + 1) others )
     | Sexp_array, [ sexp ] ->
       ( array_of_sexp conv sexp
       , parse_spine_fast ~fields:rest ~index ~extra ~seen:(seen + 1) others )
     | Sexp_bool, [] ->
       true, parse_spine_fast ~fields:rest ~index ~extra ~seen:(seen + 1) others
     (* malformed field of some kind, dispatch to slow path *)
     | _, _ -> (parse_record_slow [@tailcall false]) ~fields ~index ~extra ~seen sexps)
  (* malformed or out-of-order field, dispatch to slow path *)
  | _ -> (parse_record_slow [@tailcall false]) ~fields ~index ~extra ~seen sexps

and[@tail_mod_cons] parse_spine_fast
  : type a.
    fields:a Fields.t
    -> index:(string -> int)
    -> extra:bool
    -> seen:int
    -> Sexp.t list
    -> a
  =
  fun ~fields ~index ~extra ~seen sexps ->
  match fields with
  | Field _ -> parse_field_fast ~fields ~index ~extra ~seen sexps
  | Empty ->
    (match sexps with
     | [] -> ()
     | _ :: _ ->
       (* extra sexps, dispatch to slow path *)
       (parse_record_slow [@tailcall false]) ~fields ~index ~extra ~seen sexps)
;;

let parse_record_fast ~fields ~index ~extra sexps =
  parse_spine_fast ~fields ~index ~extra ~seen:0 sexps
;;

(* Entry points. *)

let record_of_sexps
  ~caller
  ~context
  ~fields
  ~index_of_field
  ~allow_extra_fields
  ~create
  sexps
  =
  let allow_extra_fields =
    allow_extra_fields || not (Dynamic.get Sexp_conv.record_check_extra_fields)
  in
  match
    parse_record_fast ~fields ~index:index_of_field ~extra:allow_extra_fields sexps
  with
  | value -> create value
  | exception Malformed malformed -> Malformed.raise malformed ~caller ~context
;;

let record_of_sexp ~caller ~fields ~index_of_field ~allow_extra_fields ~create sexp =
  match (sexp : Sexp.t) with
  | Atom _ as context -> record_list_instead_atom caller context
  | List sexps as context ->
    record_of_sexps
      ~caller
      ~context
      ~fields
      ~index_of_field
      ~allow_extra_fields
      ~create
      sexps
;;
