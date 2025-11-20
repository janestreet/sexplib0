open! StdLabels

module Buffer = struct
  include Buffer

  external magic_global : 'a @ local -> 'b @@ portable = "%identity"

  let add_string t str = Buffer.add_string (magic_global t) (magic_global str)

  let blit src srcoff dst dstoff len =
    Buffer.blit (magic_global src) srcoff (magic_global dst) dstoff len
  ;;
end

module Bytes = struct
  include Bytes

  external create__stack : int -> bytes @ local @@ portable = "caml_create_local_bytes"

  external unsafe_set
    :  (bytes[@local_opt])
    -> int
    -> char
    -> unit
    @@ portable
    = "%bytes_unsafe_set"

  external unsafe_to_string
    :  (bytes[@local_opt])
    -> (string[@local_opt])
    @@ portable
    = "%bytes_to_string"

  external unsafe_blit_string
    :  src:(string[@local_opt])
    -> src_pos:int
    -> dst:(bytes[@local_opt])
    -> dst_pos:int
    -> len:int
    -> unit
    @@ portable
    = "caml_blit_string"
  [@@noalloc]
end

module String = struct
  include String

  let rec index_rec_opt s lim i c =
    if i >= lim
    then None
    else if unsafe_get s i = c
    then Some i
    else index_rec_opt s lim (i + 1) c
  ;;

  let index_from_opt s i c =
    let l = length s in
    if i < 0 || i > l
    then invalid_arg "String.index_from_opt / Bytes.index_from_opt"
    else index_rec_opt s l i c
  ;;
end
