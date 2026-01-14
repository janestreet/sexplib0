include Sexp_intf.Definitions

external globalize_string : string -> string = "caml_obj_dup"

let rec globalize : t -> t = function
  | Atom atom -> Atom (globalize_string atom)
  | List list -> List (globalize_list list)

and[@tail_mod_cons] globalize_list : t list -> t list = function
  | [] -> []
  | sexp :: list -> globalize sexp :: globalize_list list
;;

external is_stack : t -> bool = "%obj_is_int"
external magic_global : t -> t = "%identity"

let maybe_globalize t = if is_stack t then globalize t else magic_global t
