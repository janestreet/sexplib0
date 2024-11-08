module type S_any = sig
  type t : any

  val t_of_sexp : Sexp.t -> t
  val sexp_of_t : t -> Sexp.t
end

module type S = sig
  type t

  include S_any with type t := t
end

module type S_any1 = sig
  type 'a t : any

  val t_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a t
  val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t
end

module type S1 = sig
  type 'a t

  include S_any1 with type 'a t := 'a t
end

module type S_any2 = sig
  type ('a, 'b) t : any

  val t_of_sexp : (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) t
  val sexp_of_t : ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('a, 'b) t -> Sexp.t
end

module type S2 = sig
  type ('a, 'b) t

  include S_any2 with type ('a, 'b) t := ('a, 'b) t
end

module type S_any3 = sig
  type ('a, 'b, 'c) t : any

  val t_of_sexp
    :  (Sexp.t -> 'a)
    -> (Sexp.t -> 'b)
    -> (Sexp.t -> 'c)
    -> Sexp.t
    -> ('a, 'b, 'c) t

  val sexp_of_t
    :  ('a -> Sexp.t)
    -> ('b -> Sexp.t)
    -> ('c -> Sexp.t)
    -> ('a, 'b, 'c) t
    -> Sexp.t
end

module type S3 = sig
  type ('a, 'b, 'c) t

  include S_any3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
end

module type S_with_grammar = sig
  include S

  val t_sexp_grammar : t Sexp_grammar.t
end

module type S1_with_grammar = sig
  include S1

  val t_sexp_grammar : 'a Sexp_grammar.t -> 'a t Sexp_grammar.t
end

module type S2_with_grammar = sig
  include S2

  val t_sexp_grammar : 'a Sexp_grammar.t -> 'b Sexp_grammar.t -> ('a, 'b) t Sexp_grammar.t
end

module type S3_with_grammar = sig
  include S3

  val t_sexp_grammar
    :  'a Sexp_grammar.t
    -> 'b Sexp_grammar.t
    -> 'c Sexp_grammar.t
    -> ('a, 'b, 'c) t Sexp_grammar.t
end
