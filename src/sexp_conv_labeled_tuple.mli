(* Parses sexps for labeled tuples, a language feature currently only implemented in Jane
   Street's experimental branch of the compiler
   (https://github.com/ocaml-flambda/flambda-backend/). *)

module Layout_witness : sig
  type _ t =
    | Value : _ t
    | Any : 'a. (unit -> 'a) t
end

module Fields : sig
  type _ t =
    | Field :
        { name : string
        ; layout : 'a Layout_witness.t
        ; conv : Sexp.t -> 'a
        ; rest : 'b t
        }
        -> ('a * 'b) t
    | Empty : unit t
end

val labeled_tuple_of_sexp
  :  caller:string
  -> fields:'a Fields.t
  -> create:('a -> 'b)
  -> Sexp.t
  -> 'b
