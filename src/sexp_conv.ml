(* Utility Module for S-expression Conversions *)

open StdLabels
open MoreLabels
open Basement

open Blocking_sync [@@alert
                     "-deprecated"
                     (* Used here since sexplib0 can't depend on Await_sync *)]

open Printf
open Sexp

(* Conversion of OCaml-values to S-expressions *)

external globalize_float : local_ float -> float @@ portable = "%obj_dup"
external bytes_length : local_ bytes -> int @@ portable = "%bytes_length"
external create_local_bytes : int -> local_ bytes @@ portable = "caml_create_local_bytes"

external unsafe_blit_bytes
  :  src:local_ bytes
  -> src_pos:int
  -> dst:local_ bytes
  -> dst_pos:int
  -> len:int
  -> unit
  @@ portable
  = "caml_blit_bytes"
[@@noalloc]

external unsafe_bytes_to_string
  :  local_ bytes
  -> local_ string
  @@ portable
  = "%bytes_to_string"

let bytes_to_string_local b = exclave_
  let len = bytes_length b in
  let s = create_local_bytes len in
  unsafe_blit_bytes ~src:b ~src_pos:0 ~dst:s ~dst_pos:0 ~len;
  unsafe_bytes_to_string s
;;

external unsafe_fill_bytes
  :  local_ bytes
  -> pos:int
  -> len:int
  -> char
  -> unit
  @@ portable
  = "caml_fill_bytes"
[@@noalloc]

let string_make_local n c = exclave_
  let s = create_local_bytes n in
  unsafe_fill_bytes s ~pos:0 ~len:n c;
  unsafe_bytes_to_string s
;;

external format_float : string -> local_ float -> string @@ portable = "caml_format_float"
external format_int32 : string -> local_ int32 -> string @@ portable = "caml_int32_format"
external format_int64 : string -> local_ int64 -> string @@ portable = "caml_int64_format"

external format_nativeint
  :  string
  -> local_ nativeint
  -> string
  @@ portable
  = "caml_nativeint_format"

external lazy_force
  :  ('a lazy_t[@local_opt])
  -> ('a[@local_opt])
  @@ portable
  = "%lazy_force"

external array_length : local_ _ array -> int @@ portable = "%array_length"

external array_safe_get
  :  ('a array[@local_opt])
  -> int
  -> ('a[@local_opt])
  @@ portable
  = "%array_safe_get"

let string_of_int32 n = format_int32 "%d" n
let string_of_int64 n = format_int64 "%d" n
let string_of_nativeint n = format_nativeint "%d" n

(* '%.17g' is guaranteed to be round-trippable.

   '%.15g' will be round-trippable and not have noise at the last digit or two for a float
   which was converted from a decimal (string) with <= 15 significant digits.  So it's
   worth trying first to avoid things like "3.1400000000000001".

   See comment above [to_string_round_trippable] in {!Core.Float} for
   detailed explanation and examples. *)
let default_string_of_float =
  Dynamic.make (fun x ->
    let y = format_float "%.15G" x in
    if float_of_string y = x then y else format_float "%.17G" x)
;;

let read_old_option_format = Dynamic.make true
let write_old_option_format = Dynamic.make true
let list_map f l = List.map l ~f

let list_map__stack f lst = exclave_
  let rec rev lst acc = exclave_
    match lst with
    | [] -> acc
    | hd :: tl -> rev tl (hd :: acc)
  in
  let rec rev_map lst acc = exclave_
    match lst with
    | [] -> acc
    | hd :: tl -> rev_map tl (f hd :: acc)
  in
  rev (rev_map lst []) []
;;

let sexp_of_unit () = List []
let sexp_of_unit__stack () = exclave_ List []

let[@zero_alloc] sexp_of_bool = function
  | false -> Atom "false"
  | true -> Atom "true"
;;

let sexp_of_bool__stack = sexp_of_bool
let sexp_of_string str = Atom str
let sexp_of_string__stack str = exclave_ Atom str
let sexp_of_bytes bytes = Atom (Bytes.to_string bytes)
let sexp_of_bytes__stack bytes = exclave_ Atom (bytes_to_string_local bytes)
let sexp_of_int n = Atom (string_of_int n)
let sexp_of_int__stack n = exclave_ Atom (string_of_int n)
let sexp_of_float n = Atom ((Dynamic.get default_string_of_float) n)

let sexp_of_float__stack n = exclave_
  Atom ((Dynamic.get default_string_of_float) (globalize_float n))
;;

let sexp_of_int32 n = Atom (Int32.to_string n)
let sexp_of_int32__stack n = exclave_ Atom (string_of_int32 n)
let sexp_of_int64 n = Atom (Int64.to_string n)
let sexp_of_int64__stack n = exclave_ Atom (string_of_int64 n)
let sexp_of_nativeint n = Atom (Nativeint.to_string n)
let sexp_of_nativeint__stack n = exclave_ Atom (string_of_nativeint n)
let sexp_of_ref sexp_of__a rf = sexp_of__a !rf
let sexp_of_ref__stack sexp_of__a rf = exclave_ sexp_of__a !rf
let sexp_of_lazy_t sexp_of__a lv = sexp_of__a (Lazy.force lv)
let sexp_of_lazy_t__stack sexp_of__a lv = exclave_ sexp_of__a (lazy_force lv)

let sexp_of_option sexp_of__a option =
  let write_old_option_format = Dynamic.get write_old_option_format in
  match option with
  | Some x when write_old_option_format -> List [ sexp_of__a x ]
  | Some x -> List [ Atom "some"; sexp_of__a x ]
  | None when write_old_option_format -> List []
  | None -> Atom "none"
;;

let sexp_of_option__stack sexp_of__a option =
  let write_old_option_format = Dynamic.get write_old_option_format in
  match option with
  | Some x when write_old_option_format -> exclave_ List [ sexp_of__a x ]
  | Some x -> exclave_ List [ Atom "some"; sexp_of__a x ]
  | None when write_old_option_format -> exclave_ List []
  | None -> exclave_ Atom "none"
;;

let sexp_of_or_null sexp_of__a or_null =
  let write_old_option_format = Dynamic.get write_old_option_format in
  match or_null with
  | Or_null_shim.This x when write_old_option_format -> List [ sexp_of__a x ]
  | Or_null_shim.This x -> List [ Atom "this"; sexp_of__a x ]
  | Null when write_old_option_format -> List []
  | Null -> Atom "null"
;;

let sexp_of_or_null__stack sexp_of__a or_null =
  let write_old_option_format = Dynamic.get write_old_option_format in
  match or_null with
  | Or_null_shim.This x when write_old_option_format -> exclave_ List [ sexp_of__a x ]
  | Or_null_shim.This x -> exclave_ List [ Atom "this"; sexp_of__a x ]
  | Null when write_old_option_format -> exclave_ List []
  | Null -> exclave_ Atom "null"
;;

let sexp_of_pair sexp_of__a sexp_of__b (a, b) = List [ sexp_of__a a; sexp_of__b b ]

let sexp_of_triple sexp_of__a sexp_of__b sexp_of__c (a, b, c) =
  List [ sexp_of__a a; sexp_of__b b; sexp_of__c c ]
;;

let sexp_of_list sexp_of__a lst = List (List.map lst ~f:sexp_of__a)
let sexp_of_list__stack sexp_of__a lst = exclave_ List (list_map__stack sexp_of__a lst)

let sexp_of_array sexp_of__a ar =
  let lst_ref = ref [] in
  for i = Array.length ar - 1 downto 0 do
    lst_ref := sexp_of__a ar.(i) :: !lst_ref
  done;
  List !lst_ref
;;

let sexp_of_array__stack sexp_of__a ar = exclave_
  let rec loop i acc = exclave_
    if i < 0 then List acc else loop (i - 1) (sexp_of__a (array_safe_get ar i) :: acc)
  in
  loop (array_length ar - 1) []
;;

let sexp_of_hashtbl sexp_of_key sexp_of_val htbl =
  let coll ~key:k ~data:v acc = List [ sexp_of_key k; sexp_of_val v ] :: acc in
  List (Hashtbl.fold htbl ~init:[] ~f:coll)
;;

let sexp_of_opaque _ = Atom "<opaque>"
let sexp_of_fun _ = Atom "<fun>"

(* Exception converter registration and lookup *)

module Exn_converter = struct
  (* Fast and automatic exception registration *)

  module Registration = struct
    type t : value mod contended portable =
      { sexp_of_exn : exn -> Sexp.t @@ portable
      ; (* If [printexc = true] then this sexp converter is used for Printexc.to_string *)
        printexc : bool
      }
    [@@unsafe_allow_any_mode_crossing]
  end

  module Exn_table = Basement.Stdlib_shim.Ephemeron.K1.MakePortable (struct
      type t = extension_constructor

      let equal = ( == )
      let hash = Obj.Extension_constructor.id
    end)

  module type The_exn_table = sig
    type key

    val lock : key Mutex.t
  end

  module The_exn_table : The_exn_table =
    (val let (Capsule.Key.P (type key) (key : key Capsule.Key.t)) = Capsule.create () in
         let lock = Mutex.create key in
         (module struct
           type nonrec key = key

           let lock = lock
         end : The_exn_table))

  let the_exn_table : (Registration.t Exn_table.t, The_exn_table.key) Capsule.Data.t =
    Capsule.Data.create (fun () -> Exn_table.create 17)
  ;;

  (* Ephemerons are used so that [sexp_of_exn] closure don't keep the
     extension_constructor live. *)
  let add ?(printexc = true) ?finalise:_ extension_constructor sexp_of_exn =
    let sexp_of_exn = Portability_hacks.magic_portable__needs_base_and_core sexp_of_exn in
    let extension_constructor =
      Portability_hacks.Cross.Portable.(cross extension_constructor) extension_constructor
    in
    Mutex.with_lock The_exn_table.lock ~f:(fun password ->
      Capsule.Data.iter the_exn_table ~password ~f:(fun the_exn_table ->
        let extension_constructor =
          Portability_hacks.Cross.Contended.(cross extension_constructor)
            extension_constructor
        in
        Exn_table.add
          the_exn_table
          extension_constructor
          ({ sexp_of_exn; printexc } : Registration.t)))
  ;;

  let find_auto ~for_printexc exn =
    let extension_constructor = Obj.Extension_constructor.of_val exn in
    let extension_constructor =
      Portability_hacks.Cross.Portable.(cross extension_constructor) extension_constructor
    in
    match
      Mutex.with_lock The_exn_table.lock ~f:(fun password ->
        Capsule.Data.extract the_exn_table ~password ~f:(fun the_exn_table ->
          let extension_constructor =
            Portability_hacks.Cross.Contended.(cross extension_constructor)
              extension_constructor
          in
          { Stdlib_shim.Modes.Aliased.aliased =
              (Exn_table.find_opt the_exn_table extension_constructor
               : Registration.t option)
          })
        [@nontail])
    with
    | { aliased = None } -> None
    | { aliased = Some { sexp_of_exn; printexc } } ->
      (match for_printexc, printexc with
       | false, _ | _, true -> Some (sexp_of_exn exn)
       | true, false -> None)
  ;;

  module For_unit_tests_only = struct
    let size () =
      Mutex.with_lock The_exn_table.lock ~f:(fun password ->
        Capsule.Data.extract the_exn_table ~password ~f:(fun the_exn_table ->
          (Exn_table.stats_alive the_exn_table).num_bindings))
    ;;
  end
end

let sexp_of_exn_opt_for_printexc exn = Exn_converter.find_auto ~for_printexc:true exn
let sexp_of_exn_opt exn = Exn_converter.find_auto ~for_printexc:false exn

let sexp_of_exn exn =
  match sexp_of_exn_opt exn with
  | None -> List [ Atom (Printexc.to_string exn) ]
  | Some sexp -> sexp
;;

let exn_to_string e = Sexp.to_string_hum (sexp_of_exn e)

(* {[exception Blah [@@deriving sexp]]} generates a call to the function
   [Exn_converter.add] defined in this file.  So we are guaranted that as soon as we
   mark an exception as sexpable, this module will be linked in and this printer will be
   registered, which is what we want. *)
let () =
  (Printexc.register_printer [@alert "-unsafe_multidomain"]) (fun exn ->
    match sexp_of_exn_opt_for_printexc exn with
    | None -> None
    | Some sexp -> Some (Sexp.to_string_hum ~indent:2 sexp))
;;

let printexc_prefer_sexp exn =
  match sexp_of_exn_opt exn with
  | None -> Printexc.to_string exn
  | Some sexp -> Sexp.to_string_hum ~indent:2 sexp
;;

(* Conversion of S-expressions to OCaml-values *)

exception Of_sexp_error = Sexp.Of_sexp_error

let record_check_extra_fields = Dynamic.make true
let of_sexp_error_exn exc sexp = raise (Of_sexp_error (exc, sexp))
let of_sexp_error what sexp = raise (Of_sexp_error (Failure what, sexp))

let unit_of_sexp sexp =
  match sexp with
  | List [] -> ()
  | Atom _ | List _ -> of_sexp_error "unit_of_sexp: empty list needed" sexp
;;

let bool_of_sexp sexp =
  match sexp with
  | Atom ("true" | "True") -> true
  | Atom ("false" | "False") -> false
  | Atom _ -> of_sexp_error "bool_of_sexp: unknown string" sexp
  | List _ -> of_sexp_error "bool_of_sexp: atom needed" sexp
;;

let string_of_sexp sexp =
  match sexp with
  | Atom str -> str
  | List _ -> of_sexp_error "string_of_sexp: atom needed" sexp
;;

let bytes_of_sexp sexp =
  match sexp with
  | Atom str -> Bytes.of_string str
  | List _ -> of_sexp_error "bytes_of_sexp: atom needed" sexp
;;

let char_of_sexp sexp =
  match sexp with
  | Atom str ->
    if String.length str <> 1
    then of_sexp_error "char_of_sexp: atom string must contain one character only" sexp;
    str.[0]
  | List _ -> of_sexp_error "char_of_sexp: atom needed" sexp
;;

let int_of_sexp sexp =
  match sexp with
  | Atom str ->
    (try int_of_string str with
     | exc -> of_sexp_error ("int_of_sexp: " ^ exn_to_string exc) sexp)
  | List _ -> of_sexp_error "int_of_sexp: atom needed" sexp
;;

let float_of_sexp sexp =
  match sexp with
  | Atom str ->
    (try float_of_string str with
     | exc -> of_sexp_error ("float_of_sexp: " ^ exn_to_string exc) sexp)
  | List _ -> of_sexp_error "float_of_sexp: atom needed" sexp
;;

let int32_of_sexp sexp =
  match sexp with
  | Atom str ->
    (try Int32.of_string str with
     | exc -> of_sexp_error ("int32_of_sexp: " ^ exn_to_string exc) sexp)
  | List _ -> of_sexp_error "int32_of_sexp: atom needed" sexp
;;

let int64_of_sexp sexp =
  match sexp with
  | Atom str ->
    (try Int64.of_string str with
     | exc -> of_sexp_error ("int64_of_sexp: " ^ exn_to_string exc) sexp)
  | List _ -> of_sexp_error "int64_of_sexp: atom needed" sexp
;;

let nativeint_of_sexp sexp =
  match sexp with
  | Atom str ->
    (try Nativeint.of_string str with
     | exc -> of_sexp_error ("nativeint_of_sexp: " ^ exn_to_string exc) sexp)
  | List _ -> of_sexp_error "nativeint_of_sexp: atom needed" sexp
;;

let ref_of_sexp a__of_sexp sexp = ref (a__of_sexp sexp)
let lazy_t_of_sexp a__of_sexp sexp = Lazy.from_val (a__of_sexp sexp)

let option_of_sexp a__of_sexp sexp =
  if Dynamic.get read_old_option_format
  then (
    match sexp with
    | List [] | Atom ("none" | "None") -> None
    | List [ el ] | List [ Atom ("some" | "Some"); el ] -> Some (a__of_sexp el)
    | List _ -> of_sexp_error "option_of_sexp: list must represent optional value" sexp
    | Atom _ -> of_sexp_error "option_of_sexp: only none can be atom" sexp)
  else (
    match sexp with
    | Atom ("none" | "None") -> None
    | List [ Atom ("some" | "Some"); el ] -> Some (a__of_sexp el)
    | Atom _ -> of_sexp_error "option_of_sexp: only none can be atom" sexp
    | List _ -> of_sexp_error "option_of_sexp: list must be (some el)" sexp)
;;

let or_null_of_sexp a__of_sexp sexp =
  if Dynamic.get read_old_option_format
  then (
    match sexp with
    | List [] | Atom ("null" | "Null") -> Or_null_shim.Null
    | List [ el ] | List [ Atom ("this" | "This"); el ] -> This (a__of_sexp el)
    | List _ -> of_sexp_error "or_null_of_sexp: list must represent or_null value" sexp
    | Atom _ -> of_sexp_error "or_null_of_sexp: only null can be atom" sexp)
  else (
    match sexp with
    | Atom ("null" | "Null") -> Or_null_shim.Null
    | List [ Atom ("this" | "This"); el ] -> This (a__of_sexp el)
    | Atom _ -> of_sexp_error "or_null_of_sexp: only null can be atom" sexp
    | List _ -> of_sexp_error "or_null_of_sexp: list must be (this el)" sexp)
;;

let pair_of_sexp a__of_sexp b__of_sexp sexp =
  match sexp with
  | List [ a_sexp; b_sexp ] ->
    let a = a__of_sexp a_sexp in
    let b = b__of_sexp b_sexp in
    a, b
  | List _ ->
    of_sexp_error "pair_of_sexp: list must contain exactly two elements only" sexp
  | Atom _ -> of_sexp_error "pair_of_sexp: list needed" sexp
;;

let triple_of_sexp a__of_sexp b__of_sexp c__of_sexp sexp =
  match sexp with
  | List [ a_sexp; b_sexp; c_sexp ] ->
    let a = a__of_sexp a_sexp in
    let b = b__of_sexp b_sexp in
    let c = c__of_sexp c_sexp in
    a, b, c
  | List _ ->
    of_sexp_error "triple_of_sexp: list must contain exactly three elements only" sexp
  | Atom _ -> of_sexp_error "triple_of_sexp: list needed" sexp
;;

let list_of_sexp a__of_sexp sexp =
  match sexp with
  | List lst -> List.map lst ~f:a__of_sexp
  | Atom _ -> of_sexp_error "list_of_sexp: list needed" sexp
;;

let array_of_sexp a__of_sexp sexp =
  match sexp with
  | List [] -> [||]
  | List (h :: t) ->
    let len = List.length t + 1 in
    let res = Array.make len (a__of_sexp h) in
    let rec loop i = function
      | [] -> res
      | h :: t ->
        res.(i) <- a__of_sexp h;
        loop (i + 1) t
    in
    loop 1 t
  | Atom _ -> of_sexp_error "array_of_sexp: list needed" sexp
;;

let hashtbl_of_sexp key_of_sexp val_of_sexp sexp =
  match sexp with
  | List lst ->
    let htbl = Hashtbl.create 0 in
    let act = function
      | List [ k_sexp; v_sexp ] ->
        Hashtbl.add htbl ~key:(key_of_sexp k_sexp) ~data:(val_of_sexp v_sexp)
      | List _ | Atom _ -> of_sexp_error "hashtbl_of_sexp: tuple list needed" sexp
    in
    List.iter lst ~f:act;
    htbl
  | Atom _ -> of_sexp_error "hashtbl_of_sexp: list needed" sexp
;;

let opaque_of_sexp sexp =
  of_sexp_error "opaque_of_sexp: cannot convert opaque values" sexp
;;

let fun_of_sexp sexp = of_sexp_error "fun_of_sexp: cannot convert function values" sexp

(* Sexp Grammars *)

include Sexp_conv_grammar

(* Registering default exception printers *)

let get_flc_error name (file, line, chr) = Atom (sprintf "%s %s:%d:%d" name file line chr)

type handler : value mod portable = { h : exn -> Sexp.t @@ portable }
[@@unboxed] [@@unsafe_allow_any_mode_crossing]

let () =
  List.iter
    ~f:(fun (extension_constructor, handler) ->
      Exn_converter.add ~printexc:false ~finalise:false extension_constructor handler.h)
    [ ( [%extension_constructor Assert_failure]
      , { h =
            (function
              | Assert_failure arg -> get_flc_error "Assert_failure" arg
              | _ -> assert false)
        } )
    ; ( [%extension_constructor Exit]
      , { h =
            (function
              | Exit -> Atom "Exit"
              | _ -> assert false)
        } )
    ; ( [%extension_constructor End_of_file]
      , { h =
            (function
              | End_of_file -> Atom "End_of_file"
              | _ -> assert false)
        } )
    ; ( [%extension_constructor Failure]
      , { h =
            (function
              | Failure arg -> List [ Atom "Failure"; Atom arg ]
              | _ -> assert false)
        } )
    ; ( [%extension_constructor Not_found]
      , { h =
            (function
              | Not_found -> Atom "Not_found"
              | _ -> assert false)
        } )
    ; ( [%extension_constructor Invalid_argument]
      , { h =
            (function
              | Invalid_argument arg -> List [ Atom "Invalid_argument"; Atom arg ]
              | _ -> assert false)
        } )
    ; ( [%extension_constructor Match_failure]
      , { h =
            (function
              | Match_failure arg -> get_flc_error "Match_failure" arg
              | _ -> assert false)
        } )
    ; ( [%extension_constructor Not_found_s]
      , { h =
            (function
              | Not_found_s arg -> List [ Atom "Not_found_s"; arg ]
              | _ -> assert false)
        } )
    ; ( [%extension_constructor Sys_error]
      , { h =
            (function
              | Sys_error arg -> List [ Atom "Sys_error"; Atom arg ]
              | _ -> assert false)
        } )
    ; ( [%extension_constructor Arg.Help]
      , { h =
            (function
              | Arg.Help arg -> List [ Atom "Arg.Help"; Atom arg ]
              | _ -> assert false)
        } )
    ; ( [%extension_constructor Arg.Bad]
      , { h =
            (function
              | Arg.Bad arg -> List [ Atom "Arg.Bad"; Atom arg ]
              | _ -> assert false)
        } )
    ; ( [%extension_constructor Lazy.Undefined]
      , { h =
            (function
              | Lazy.Undefined -> Atom "Lazy.Undefined"
              | _ -> assert false)
        } )
    ; ( [%extension_constructor Parsing.Parse_error]
      , { h =
            (function
              | Parsing.Parse_error -> Atom "Parsing.Parse_error"
              | _ -> assert false)
        } )
    ; ( [%extension_constructor Queue.Empty]
      , { h =
            (function
              | Queue.Empty -> Atom "Queue.Empty"
              | _ -> assert false)
        } )
    ; ( [%extension_constructor Scanf.Scan_failure]
      , { h =
            (function
              | Scanf.Scan_failure arg -> List [ Atom "Scanf.Scan_failure"; Atom arg ]
              | _ -> assert false)
        } )
    ; ( [%extension_constructor Stack.Empty]
      , { h =
            (function
              | Stack.Empty -> Atom "Stack.Empty"
              | _ -> assert false)
        } )
    ; ( [%extension_constructor Sys.Break]
      , { h =
            (function
              | Sys.Break -> Atom "Sys.Break"
              | _ -> assert false)
        } )
    ]
;;

let () =
  List.iter
    ~f:(fun (extension_constructor, handler) ->
      Exn_converter.add ~printexc:true ~finalise:false extension_constructor handler.h)
    [ ( [%extension_constructor Of_sexp_error]
      , { h =
            (function
              | Of_sexp_error (exc, sexp) ->
                List [ Atom "Sexplib.Conv.Of_sexp_error"; sexp_of_exn exc; sexp ]
              | _ -> assert false)
        } )
    ]
;;

external ignore : ('a : value_or_null). ('a[@local_opt]) -> unit @@ portable = "%ignore"

external ( = )
  : ('a : value_or_null).
  ('a[@local_opt]) -> ('a[@local_opt]) -> bool
  @@ portable
  = "%equal"

(* The compiler generates *catastrophically* bad code if you let it inline this function.
   But with that prevented, the compiler reliably optimizes this to a load from a
   statically allocated array. *)
let[@zero_alloc] [@inline never] [@local never] [@specialise never] sexp_of_char_statically_allocated
  = function
  (*$
    for i = 0 to 255 do
      Printf.printf "| '\\x%02x' -> Atom \"\\x%02x\"\n" i i
    done
  *)
  | '\x00' -> Atom "\x00"
  | '\x01' -> Atom "\x01"
  | '\x02' -> Atom "\x02"
  | '\x03' -> Atom "\x03"
  | '\x04' -> Atom "\x04"
  | '\x05' -> Atom "\x05"
  | '\x06' -> Atom "\x06"
  | '\x07' -> Atom "\x07"
  | '\x08' -> Atom "\x08"
  | '\x09' -> Atom "\x09"
  | '\x0a' -> Atom "\x0a"
  | '\x0b' -> Atom "\x0b"
  | '\x0c' -> Atom "\x0c"
  | '\x0d' -> Atom "\x0d"
  | '\x0e' -> Atom "\x0e"
  | '\x0f' -> Atom "\x0f"
  | '\x10' -> Atom "\x10"
  | '\x11' -> Atom "\x11"
  | '\x12' -> Atom "\x12"
  | '\x13' -> Atom "\x13"
  | '\x14' -> Atom "\x14"
  | '\x15' -> Atom "\x15"
  | '\x16' -> Atom "\x16"
  | '\x17' -> Atom "\x17"
  | '\x18' -> Atom "\x18"
  | '\x19' -> Atom "\x19"
  | '\x1a' -> Atom "\x1a"
  | '\x1b' -> Atom "\x1b"
  | '\x1c' -> Atom "\x1c"
  | '\x1d' -> Atom "\x1d"
  | '\x1e' -> Atom "\x1e"
  | '\x1f' -> Atom "\x1f"
  | '\x20' -> Atom "\x20"
  | '\x21' -> Atom "\x21"
  | '\x22' -> Atom "\x22"
  | '\x23' -> Atom "\x23"
  | '\x24' -> Atom "\x24"
  | '\x25' -> Atom "\x25"
  | '\x26' -> Atom "\x26"
  | '\x27' -> Atom "\x27"
  | '\x28' -> Atom "\x28"
  | '\x29' -> Atom "\x29"
  | '\x2a' -> Atom "\x2a"
  | '\x2b' -> Atom "\x2b"
  | '\x2c' -> Atom "\x2c"
  | '\x2d' -> Atom "\x2d"
  | '\x2e' -> Atom "\x2e"
  | '\x2f' -> Atom "\x2f"
  | '\x30' -> Atom "\x30"
  | '\x31' -> Atom "\x31"
  | '\x32' -> Atom "\x32"
  | '\x33' -> Atom "\x33"
  | '\x34' -> Atom "\x34"
  | '\x35' -> Atom "\x35"
  | '\x36' -> Atom "\x36"
  | '\x37' -> Atom "\x37"
  | '\x38' -> Atom "\x38"
  | '\x39' -> Atom "\x39"
  | '\x3a' -> Atom "\x3a"
  | '\x3b' -> Atom "\x3b"
  | '\x3c' -> Atom "\x3c"
  | '\x3d' -> Atom "\x3d"
  | '\x3e' -> Atom "\x3e"
  | '\x3f' -> Atom "\x3f"
  | '\x40' -> Atom "\x40"
  | '\x41' -> Atom "\x41"
  | '\x42' -> Atom "\x42"
  | '\x43' -> Atom "\x43"
  | '\x44' -> Atom "\x44"
  | '\x45' -> Atom "\x45"
  | '\x46' -> Atom "\x46"
  | '\x47' -> Atom "\x47"
  | '\x48' -> Atom "\x48"
  | '\x49' -> Atom "\x49"
  | '\x4a' -> Atom "\x4a"
  | '\x4b' -> Atom "\x4b"
  | '\x4c' -> Atom "\x4c"
  | '\x4d' -> Atom "\x4d"
  | '\x4e' -> Atom "\x4e"
  | '\x4f' -> Atom "\x4f"
  | '\x50' -> Atom "\x50"
  | '\x51' -> Atom "\x51"
  | '\x52' -> Atom "\x52"
  | '\x53' -> Atom "\x53"
  | '\x54' -> Atom "\x54"
  | '\x55' -> Atom "\x55"
  | '\x56' -> Atom "\x56"
  | '\x57' -> Atom "\x57"
  | '\x58' -> Atom "\x58"
  | '\x59' -> Atom "\x59"
  | '\x5a' -> Atom "\x5a"
  | '\x5b' -> Atom "\x5b"
  | '\x5c' -> Atom "\x5c"
  | '\x5d' -> Atom "\x5d"
  | '\x5e' -> Atom "\x5e"
  | '\x5f' -> Atom "\x5f"
  | '\x60' -> Atom "\x60"
  | '\x61' -> Atom "\x61"
  | '\x62' -> Atom "\x62"
  | '\x63' -> Atom "\x63"
  | '\x64' -> Atom "\x64"
  | '\x65' -> Atom "\x65"
  | '\x66' -> Atom "\x66"
  | '\x67' -> Atom "\x67"
  | '\x68' -> Atom "\x68"
  | '\x69' -> Atom "\x69"
  | '\x6a' -> Atom "\x6a"
  | '\x6b' -> Atom "\x6b"
  | '\x6c' -> Atom "\x6c"
  | '\x6d' -> Atom "\x6d"
  | '\x6e' -> Atom "\x6e"
  | '\x6f' -> Atom "\x6f"
  | '\x70' -> Atom "\x70"
  | '\x71' -> Atom "\x71"
  | '\x72' -> Atom "\x72"
  | '\x73' -> Atom "\x73"
  | '\x74' -> Atom "\x74"
  | '\x75' -> Atom "\x75"
  | '\x76' -> Atom "\x76"
  | '\x77' -> Atom "\x77"
  | '\x78' -> Atom "\x78"
  | '\x79' -> Atom "\x79"
  | '\x7a' -> Atom "\x7a"
  | '\x7b' -> Atom "\x7b"
  | '\x7c' -> Atom "\x7c"
  | '\x7d' -> Atom "\x7d"
  | '\x7e' -> Atom "\x7e"
  | '\x7f' -> Atom "\x7f"
  | '\x80' -> Atom "\x80"
  | '\x81' -> Atom "\x81"
  | '\x82' -> Atom "\x82"
  | '\x83' -> Atom "\x83"
  | '\x84' -> Atom "\x84"
  | '\x85' -> Atom "\x85"
  | '\x86' -> Atom "\x86"
  | '\x87' -> Atom "\x87"
  | '\x88' -> Atom "\x88"
  | '\x89' -> Atom "\x89"
  | '\x8a' -> Atom "\x8a"
  | '\x8b' -> Atom "\x8b"
  | '\x8c' -> Atom "\x8c"
  | '\x8d' -> Atom "\x8d"
  | '\x8e' -> Atom "\x8e"
  | '\x8f' -> Atom "\x8f"
  | '\x90' -> Atom "\x90"
  | '\x91' -> Atom "\x91"
  | '\x92' -> Atom "\x92"
  | '\x93' -> Atom "\x93"
  | '\x94' -> Atom "\x94"
  | '\x95' -> Atom "\x95"
  | '\x96' -> Atom "\x96"
  | '\x97' -> Atom "\x97"
  | '\x98' -> Atom "\x98"
  | '\x99' -> Atom "\x99"
  | '\x9a' -> Atom "\x9a"
  | '\x9b' -> Atom "\x9b"
  | '\x9c' -> Atom "\x9c"
  | '\x9d' -> Atom "\x9d"
  | '\x9e' -> Atom "\x9e"
  | '\x9f' -> Atom "\x9f"
  | '\xa0' -> Atom "\xa0"
  | '\xa1' -> Atom "\xa1"
  | '\xa2' -> Atom "\xa2"
  | '\xa3' -> Atom "\xa3"
  | '\xa4' -> Atom "\xa4"
  | '\xa5' -> Atom "\xa5"
  | '\xa6' -> Atom "\xa6"
  | '\xa7' -> Atom "\xa7"
  | '\xa8' -> Atom "\xa8"
  | '\xa9' -> Atom "\xa9"
  | '\xaa' -> Atom "\xaa"
  | '\xab' -> Atom "\xab"
  | '\xac' -> Atom "\xac"
  | '\xad' -> Atom "\xad"
  | '\xae' -> Atom "\xae"
  | '\xaf' -> Atom "\xaf"
  | '\xb0' -> Atom "\xb0"
  | '\xb1' -> Atom "\xb1"
  | '\xb2' -> Atom "\xb2"
  | '\xb3' -> Atom "\xb3"
  | '\xb4' -> Atom "\xb4"
  | '\xb5' -> Atom "\xb5"
  | '\xb6' -> Atom "\xb6"
  | '\xb7' -> Atom "\xb7"
  | '\xb8' -> Atom "\xb8"
  | '\xb9' -> Atom "\xb9"
  | '\xba' -> Atom "\xba"
  | '\xbb' -> Atom "\xbb"
  | '\xbc' -> Atom "\xbc"
  | '\xbd' -> Atom "\xbd"
  | '\xbe' -> Atom "\xbe"
  | '\xbf' -> Atom "\xbf"
  | '\xc0' -> Atom "\xc0"
  | '\xc1' -> Atom "\xc1"
  | '\xc2' -> Atom "\xc2"
  | '\xc3' -> Atom "\xc3"
  | '\xc4' -> Atom "\xc4"
  | '\xc5' -> Atom "\xc5"
  | '\xc6' -> Atom "\xc6"
  | '\xc7' -> Atom "\xc7"
  | '\xc8' -> Atom "\xc8"
  | '\xc9' -> Atom "\xc9"
  | '\xca' -> Atom "\xca"
  | '\xcb' -> Atom "\xcb"
  | '\xcc' -> Atom "\xcc"
  | '\xcd' -> Atom "\xcd"
  | '\xce' -> Atom "\xce"
  | '\xcf' -> Atom "\xcf"
  | '\xd0' -> Atom "\xd0"
  | '\xd1' -> Atom "\xd1"
  | '\xd2' -> Atom "\xd2"
  | '\xd3' -> Atom "\xd3"
  | '\xd4' -> Atom "\xd4"
  | '\xd5' -> Atom "\xd5"
  | '\xd6' -> Atom "\xd6"
  | '\xd7' -> Atom "\xd7"
  | '\xd8' -> Atom "\xd8"
  | '\xd9' -> Atom "\xd9"
  | '\xda' -> Atom "\xda"
  | '\xdb' -> Atom "\xdb"
  | '\xdc' -> Atom "\xdc"
  | '\xdd' -> Atom "\xdd"
  | '\xde' -> Atom "\xde"
  | '\xdf' -> Atom "\xdf"
  | '\xe0' -> Atom "\xe0"
  | '\xe1' -> Atom "\xe1"
  | '\xe2' -> Atom "\xe2"
  | '\xe3' -> Atom "\xe3"
  | '\xe4' -> Atom "\xe4"
  | '\xe5' -> Atom "\xe5"
  | '\xe6' -> Atom "\xe6"
  | '\xe7' -> Atom "\xe7"
  | '\xe8' -> Atom "\xe8"
  | '\xe9' -> Atom "\xe9"
  | '\xea' -> Atom "\xea"
  | '\xeb' -> Atom "\xeb"
  | '\xec' -> Atom "\xec"
  | '\xed' -> Atom "\xed"
  | '\xee' -> Atom "\xee"
  | '\xef' -> Atom "\xef"
  | '\xf0' -> Atom "\xf0"
  | '\xf1' -> Atom "\xf1"
  | '\xf2' -> Atom "\xf2"
  | '\xf3' -> Atom "\xf3"
  | '\xf4' -> Atom "\xf4"
  | '\xf5' -> Atom "\xf5"
  | '\xf6' -> Atom "\xf6"
  | '\xf7' -> Atom "\xf7"
  | '\xf8' -> Atom "\xf8"
  | '\xf9' -> Atom "\xf9"
  | '\xfa' -> Atom "\xfa"
  | '\xfb' -> Atom "\xfb"
  | '\xfc' -> Atom "\xfc"
  | '\xfd' -> Atom "\xfd"
  | '\xfe' -> Atom "\xfe"
  | '\xff' -> Atom "\xff"
;;

(*$*)

let[@inline always] is_valid_char (char : char) : bool = Char.code char land lnot 0xff = 0

let[@inline never] [@local never] [@specialise never] fallback_sexp_of_char (char : char) =
  Atom ((String.make [@inlined never]) 1 char)
;;

let[@inline always] sexp_of_char (char : char) =
  if is_valid_char char
  then sexp_of_char_statically_allocated char [@tail]
  else fallback_sexp_of_char char [@tail]
;;

let[@inline never] [@local never] [@specialise never] fallback_sexp_of_char__stack
  (char : char)
  = exclave_
  Atom ((string_make_local [@inlined never]) 1 char)
;;

let[@inline always] sexp_of_char__stack (char : char) = exclave_
  if is_valid_char char
  then sexp_of_char_statically_allocated char
  else fallback_sexp_of_char__stack char
;;
