@@ portable

open! StdLabels

module Buffer : sig
  include module type of struct
    include Buffer
  end

  val add_string : t @ local -> string @ local -> unit
  val blit : t @ local -> int -> bytes @ local -> int -> int -> unit
end

module Bytes : sig
  include module type of struct
    include Bytes
  end

  external create__stack : int -> bytes @ local = "caml_create_local_bytes"
end

module String : sig
  include module type of struct
    include String
  end

  val index_from_opt : string @ local -> int -> char -> int option
end
