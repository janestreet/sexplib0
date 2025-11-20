(** Representation of S-expression grammars *)

module type Sexp_grammar = sig @@ stateless
  include module type of struct
    include Sexp_grammar_type.Sexp_grammar
  end

  (** Convert a sexp grammar for one type to another. *)
  val coerce : ('a : any) ('b : any). 'a t -> 'b t

  (** Add a key/value tag to a grammar. *)
  val tag : ('a : any). 'a t -> key:string -> value:Sexp.t -> 'a t

  (** This reserved key is used for all tags generated from doc comments. *)
  val doc_comment_tag : string

  (** This reserved key can be used to associate a type name with a grammar. *)
  val type_name_tag : string

  (** This reserved key indicates that a sexp represents a key/value association. The
      tag's value is ignored. *)
  val assoc_tag : string

  (** This reserved key indicates that a sexp is a key in a key/value association. The
      tag's value is ignored. *)
  val assoc_key_tag : string

  (** This reserved key indicates that a sexp is a value in a key/value association. The
      tag's value is ignored. *)
  val assoc_value_tag : string

  (** When the key is set to [Atom "false"] for a variant clause, that clause should not
      be suggested in auto-completion based on the sexp grammar. *)
  val completion_suggested : string
end
