@@ portable

module Kind : sig
  (** A GADT specifying how to parse a record field. See documentation for
      [ppx_sexp_conv]. *)
  type (_ : value_or_null, _) t =
    | Default : ('a : any). (unit -> 'a) -> (unit -> 'a, Sexp.t -> unit -> 'a) t
    | Omit_nil : ('a : any). (unit -> 'a, Sexp.t -> unit -> 'a) t
    | Required : ('a : any). (unit -> 'a, Sexp.t -> unit -> 'a) t
    | Sexp_array : ('a array, Sexp.t -> 'a) t
    | Sexp_bool : (bool, unit) t
    | Sexp_list : ('a list, Sexp.t -> 'a) t
    | Sexp_option : ('a option, Sexp.t -> 'a) t
    | Sexp_or_null : ('a Basement.Or_null_shim.t, Sexp.t -> 'a) t
end

module Fields : sig
  (** A GADT specifying record fields. *)

  type _ t =
    | Empty : unit t
    | Field :
        ('a : value_or_null) 'b 'conv.
        { name : string
        ; kind : ('a, 'conv) Kind.t
        ; conv : 'conv
        ; rest : 'b t
        }
        -> ('a * 'b) t
end

(** Parses a record from a sexp that must be a list of fields.

    Uses [caller] as the source for error messages. Parses using the given [field]s. Uses
    [index_of_field] to look up field names found in sexps. If [allow_extra_fields] is
    true, extra fields are allowed and discarded without error. [create] is used to
    construct the final returned value. *)
val record_of_sexp
  :  caller:string
  -> fields:'a Fields.t
  -> index_of_field:(string -> int)
  -> allow_extra_fields:bool
  -> create:('a -> 'b)
  -> Sexp.t
  -> 'b

(** Like [record_of_sexp], but for a list of sexps with no [List] wrapper. Used, for
    example, to parse arguments to a variant constructor with an inlined record argument.
    Reports [context] for parse errors when no more specific sexp is applicable. *)
val record_of_sexps
  :  caller:string
  -> context:Sexp.t
  -> fields:'a Fields.t
  -> index_of_field:(string -> int)
  -> allow_extra_fields:bool
  -> create:('a -> 'b)
  -> Sexp.t list
  -> 'b
