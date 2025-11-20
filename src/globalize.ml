include Sexp_intf.Definitions

external globalize_string : local_ string -> string @@ portable = "%obj_dup"

let rec globalize : t @ local -> t @ global = function
  | Atom atom -> Atom (globalize_string atom)
  | List list -> List (globalize_list list)

and[@tail_mod_cons] globalize_list : t list @ local -> t list @ global = function
  | [] -> []
  | sexp :: list -> globalize sexp :: globalize_list list
;;

external is_stack : t @ contended local once -> bool @@ portable = "caml_obj_is_stack"
external magic_global : t @ local -> t @ global @@ portable = "%identity"

let maybe_globalize t = if is_stack t then globalize t else magic_global t
