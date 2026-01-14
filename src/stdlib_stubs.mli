open! StdLabels

module Buffer : sig
  include module type of struct
    include Buffer
  end

  val add_string : t -> string -> unit
  val blit : t -> int -> bytes -> int -> int -> unit
end

module Bytes : sig
  include module type of struct
    include Bytes
  end

  external create__stack : int -> bytes = "caml_create_bytes"
end

module String : sig
  include module type of struct
    include String
  end

  val index_from_opt : string -> int -> char -> int option
end
