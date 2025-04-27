@@ portable

val error : loc:string -> sexp:Sexp.t -> string -> _
val simple_error : string -> string -> Sexp.t -> _

exception Of_sexp_error of exn * Sexp.t

val tuple_of_size_n_expected : string -> int -> Sexp.t -> _
val tuple_pair_expected : string -> string -> Sexp.t -> _
val stag_no_args : string -> Sexp.t -> _
val stag_incorrect_n_args : string -> string -> Sexp.t -> _
val stag_takes_args : string -> Sexp.t -> _
val nested_list_invalid_sum : string -> Sexp.t -> _
val empty_list_invalid_sum : string -> Sexp.t -> _
val unexpected_stag : string -> string list -> Sexp.t -> _
val record_sexp_bool_with_payload : string -> Sexp.t -> _
val tuple_incorrect_label : string -> string -> int -> Sexp.t -> _
val record_only_pairs_expected : string -> Sexp.t -> _
val record_invalid_fields : what:string -> loc:string -> string list -> Sexp.t -> _
val record_duplicate_fields : string -> string list -> Sexp.t -> _

val record_missing_and_extra_fields
  :  string
  -> Sexp.t
  -> missing:string list
  -> extras:string list
  -> _

val record_list_instead_atom : string -> Sexp.t -> _
val record_poly_field_value : string -> Sexp.t -> _

exception No_variant_match

val no_variant_match : unit -> _
val no_matching_variant_found : string -> Sexp.t -> _
val ptag_no_args : string -> Sexp.t -> _
val ptag_incorrect_n_args : string -> string -> Sexp.t -> _
val ptag_takes_args : string -> Sexp.t -> _
val nested_list_invalid_poly_var : string -> Sexp.t -> _
val empty_list_invalid_poly_var : string -> Sexp.t -> _
val empty_type : string -> Sexp.t -> _
