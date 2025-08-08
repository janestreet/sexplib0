open! StdLabels

module Buffer = struct
  include Buffer

  external magic_global : 'a -> 'b = "%identity"

  let add_string t str = Buffer.add_string (magic_global t) (magic_global str)

  let blit src srcoff dst dstoff len =
    Buffer.blit (magic_global src) srcoff (magic_global dst) dstoff len
  ;;
end

module Bytes = struct
  include Bytes

  external create__stack : int -> bytes = "caml_create_bytes"
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

module String = struct
  include String

  external length : (string[@local_opt]) -> int = "%string_length"
  external get : (string[@local_opt]) -> int -> char = "%string_safe_get"
  external unsafe_get : (string[@local_opt]) -> int -> char = "%string_unsafe_get"
end
