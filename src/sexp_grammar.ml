include Sexp_grammar_intf.Definitions

let coerce (type (a : any) (b : any)) ({ untyped = _ } as t : a t) : b t = t

let tag (type a : any) ({ untyped = grammar } : a t) ~key ~value : a t =
  { untyped = Tagged { key; value; grammar } }
;;

let doc_comment_tag = "sexp_grammar.doc_comment"
let type_name_tag = "sexp_grammar.type_name"
let assoc_tag = "sexp_grammar.assoc"
let assoc_key_tag = "sexp_grammar.assoc.key"
let assoc_value_tag = "sexp_grammar.assoc.value"
let completion_suggested = "sexp_grammar.completion-suggested"
