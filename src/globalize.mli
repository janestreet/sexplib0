@@ portable

type t = Sexp_intf.Definitions.t =
  | Atom of string
  | List of t list

(** [maybe_globalize] only copies [t] to the heap if it is allocated on the stack.

    This function is not free; i.e. it should not always be called in place of
    [globalize].

    It needs to make a C call to check whether the object is allocated on the stack, which
    may be expensive. *)
val maybe_globalize : t @ local -> t @ global

(** [globalize] definitely copies [t] to the heap, no matter where it is allocated. *)
val globalize : t @ local -> t @ global
