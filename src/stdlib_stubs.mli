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
  external unsafe_set : (bytes[@local_opt]) -> int -> char -> unit = "%bytes_unsafe_set"

  external unsafe_to_string
    :  (bytes[@local_opt])
    -> (string[@local_opt])
    = "%bytes_to_string"

  external unsafe_blit_string
    :  src:(string[@local_opt])
    -> src_pos:int
    -> dst:(bytes[@local_opt])
    -> dst_pos:int
    -> len:int
    -> unit
    = "caml_blit_string"
  [@@noalloc]
end

module String : sig
  include module type of struct
    include String
  end

  external length : (string[@local_opt]) -> int = "%string_length"
  external get : (string[@local_opt]) -> int -> char = "%string_safe_get"
  external unsafe_get : (string[@local_opt]) -> int -> char = "%string_unsafe_get"
end
