open Basement
open StdLabels
open Format
include Sexp_intf.Definitions

let sexp_of_t t = t
let sexp_of_t__stack t = t
let t_of_sexp t = t

let rec compare_list a b =
  match a, b with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | x :: xs, y :: ys ->
    let res = compare x y in
    if res <> 0 then res else compare_list xs ys

and compare a b =
  if a == b
  then 0
  else (
    match a, b with
    | Atom a, Atom b -> String.compare a b
    | Atom _, _ -> -1
    | _, Atom _ -> 1
    | List a, List b -> compare_list a b)
;;

let rec equal a b =
  a == b
  ||
  match a, b with
  | Atom a, Atom b -> String.equal a b
  | Atom _, _ | _, Atom _ -> false
  | List a, List b -> List.equal ~eq:equal a b
;;

exception Not_found_s of t
exception Of_sexp_error of exn * t

module Stack_buffer : sig
  (** Provides a minimal subset of [Buffer]'s capabilities, but allocating everything on
      the stack. Because of present limitations with how stack allocation works (namely
      that all mutable fields are forced to be global), this module has a "functional"
      interface, where you always must use the newly returned [t]. *)

  type t

  val create : unit -> t @ local
  val add_string : t @ local -> string @ local -> t @ local
  val add_char : t @ local -> char -> t @ local
  val length : t @ local -> int
  val unsafe_blit_all : t @ local -> bytes @ local -> unit
end = struct
  type t =
    { mutable pos : int
    ; mutable capacity : int
    ; contents : bytes
    }

  let create () = exclave_
    let capacity = 1024 in
    { pos = 0; capacity; contents = Bytes.create__stack capacity }
  ;;

  let[@inline never] [@specialise never] [@local never] resize t ~space_needed = exclave_
    (* When [space_needed] is small this is only slightly more than [t.capacity * 2], and
       when [space_needed] is large (potentially even larger than [t.capacity]!), this
       still does the right thing. *)
    let new_capacity = (t.pos + space_needed) * 2 in
    let new_contents = Bytes.create__stack new_capacity in
    Bytes.unsafe_blit ~src:t.contents ~src_pos:0 ~dst:new_contents ~dst_pos:0 ~len:t.pos;
    { pos = t.pos; capacity = new_capacity; contents = new_contents }
  ;;

  let[@inline always] maybe_resize t ~space_needed = exclave_
    if t.pos + space_needed <= t.capacity then t else resize t ~space_needed
  ;;

  let add_string t string = exclave_
    let string_length = String.length string in
    let t = maybe_resize t ~space_needed:string_length in
    Bytes.unsafe_blit_string
      ~src:string
      ~src_pos:0
      ~dst:t.contents
      ~dst_pos:t.pos
      ~len:string_length;
    t.pos <- t.pos + string_length;
    t
  ;;

  let add_char t char = exclave_
    let t = maybe_resize t ~space_needed:1 in
    Bytes.unsafe_set t.contents t.pos char;
    t.pos <- t.pos + 1;
    t
  ;;

  let length t = t.pos

  let unsafe_blit_all t bytes =
    Bytes.unsafe_blit ~src:t.contents ~src_pos:0 ~dst:bytes ~dst_pos:0 ~len:t.pos
  ;;
end

module Printing = struct
  (** Default indentation level for human-readable conversions *)
  let default_indent = Dynamic.make 1

  let index_of_newline str start = String.index_from_opt str start '\n'

  (* The maximum size of a thing on the minor heap is 256 words. Previously, this size of
     the returned buffer here was 4096 bytes, which caused the Buffer to be allocated on
     the *major* heap every time.

     According to a simple benchmark by Ron, we can improve performance for small
     s-expressions by a factor of ~4 if we only allocate 1024 bytes (128 words + some
     small overhead) worth of buffer initially. And one can argue that if it's free to
     allocate strings smaller than 256 words, large s-expressions requiring larger
     expensive buffers won't notice the extra two doublings from 1024 bytes to 2048
     and 4096. And especially performance-sensitive applications to always pass in a
     larger buffer to use. *)
  let buffer () = Buffer.create 1024

  let to_buffer_mach_internal ~buf sexp ~mach_maybe_esc_str =
    let rec loop may_need_space = function
      | Atom str ->
        let str' = mach_maybe_esc_str str in
        let new_may_need_space = str' == str in
        if may_need_space && new_may_need_space then Buffer.add_char buf ' ';
        Buffer.add_string buf str';
        new_may_need_space
      | List (h :: t) ->
        Buffer.add_char buf '(';
        let may_need_space = loop false h in
        loop_rest may_need_space t;
        false
      | List [] ->
        Buffer.add_string buf "()";
        false
    and loop_rest may_need_space = function
      | h :: t ->
        let may_need_space = loop may_need_space h in
        loop_rest may_need_space t
      | [] -> Buffer.add_char buf ')'
    in
    ignore (loop false sexp)
  ;;

  [@@@expand_inline
    [%%template
    [@@@alloc.default a @ m = (stack_local, heap_global)]

    let bytes_of_buffer buf =
      (let len = Buffer.length buf in
       let bytes = (Bytes.create [@alloc a]) len in
       Buffer.blit buf 0 bytes 0 len;
       Bytes.unsafe_to_string bytes)
      [@exclave_if_stack a]
    ;;

    let to_stack_buffer_mach_internal sexp ~mach_maybe_esc_str = exclave_
      let rec loop may_need_space sexp stack_buf ~mach_maybe_esc_str = exclave_
        match sexp with
        | Atom str ->
          let str' = mach_maybe_esc_str str in
          let new_may_need_space = str' == str in
          let stack_buf =
            if may_need_space && new_may_need_space
            then Stack_buffer.add_char stack_buf ' '
            else stack_buf
          in
          let stack_buf = Stack_buffer.add_string stack_buf str' in
          #(new_may_need_space, stack_buf)
        | List (h :: t) ->
          let stack_buf = Stack_buffer.add_char stack_buf '(' in
          let #(may_need_space, stack_buf) = loop false h stack_buf ~mach_maybe_esc_str in
          let stack_buf = loop_rest may_need_space t stack_buf ~mach_maybe_esc_str in
          #(false, stack_buf)
        | List [] ->
          let stack_buf = Stack_buffer.add_string stack_buf "()" in
          #(false, stack_buf)
      and loop_rest may_need_space sexp stack_buf ~mach_maybe_esc_str = exclave_
        match sexp with
        | h :: t ->
          let #(may_need_space, stack_buf) =
            loop may_need_space h stack_buf ~mach_maybe_esc_str
          in
          loop_rest may_need_space t stack_buf ~mach_maybe_esc_str
        | [] -> Stack_buffer.add_char stack_buf ')'
      in
      let stack_buf = Stack_buffer.create () in
      let #(_, stack_buf) = loop false sexp stack_buf ~mach_maybe_esc_str in
      stack_buf
    ;;

    let bytes_of_stack_buffer stack_buf =
      (let len = Stack_buffer.length stack_buf in
       let bytes = (Bytes.create [@alloc a]) len in
       Stack_buffer.unsafe_blit_all stack_buf bytes;
       Bytes.unsafe_to_string bytes)
      [@exclave_if_stack a]
    ;;

    let to_string_mach_internal t ~mach_maybe_esc_str =
      match t with
      | Atom str -> mach_maybe_esc_str str [@exclave_if_stack a]
      | sexp ->
        (let stack_buf =
           (to_stack_buffer_mach_internal [@alloc a]) sexp ~mach_maybe_esc_str
         in
         let result = (bytes_of_stack_buffer [@alloc a]) stack_buf in
         result)
        [@exclave_if_stack a]
    ;;

    let to_string_hum_internal
      ?indent
      ?max_width
      sexp
      ~mach_maybe_esc_str
      ~maybe_globalize
      ~to_buffer_hum
      =
      match[@exclave_if_stack a] sexp with
      | Atom str
        when match index_of_newline str 0 with
             | None -> true
             | Some _ -> false -> mach_maybe_esc_str str
      | sexp ->
        let sexp = maybe_globalize sexp in
        let buf = buffer () in
        to_buffer_hum ~buf ?indent ?max_width sexp;
        (bytes_of_buffer [@alloc a]) buf
    ;;]]

  let bytes_of_buffer__stack buf = exclave_
    let len = Buffer.length buf in
    let bytes = Bytes.create__stack len in
    Buffer.blit buf 0 bytes 0 len;
    Bytes.unsafe_to_string bytes
  ;;

  let to_stack_buffer_mach_internal__stack sexp ~mach_maybe_esc_str = exclave_
    let rec loop may_need_space sexp stack_buf ~mach_maybe_esc_str = exclave_
      match sexp with
      | Atom str ->
        let str' = mach_maybe_esc_str str in
        let new_may_need_space = str' == str in
        let stack_buf =
          if may_need_space && new_may_need_space
          then Stack_buffer.add_char stack_buf ' '
          else stack_buf
        in
        let stack_buf = Stack_buffer.add_string stack_buf str' in
        #(new_may_need_space, stack_buf)
      | List (h :: t) ->
        let stack_buf = Stack_buffer.add_char stack_buf '(' in
        let #(may_need_space, stack_buf) = loop false h stack_buf ~mach_maybe_esc_str in
        let stack_buf = loop_rest may_need_space t stack_buf ~mach_maybe_esc_str in
        #(false, stack_buf)
      | List [] ->
        let stack_buf = Stack_buffer.add_string stack_buf "()" in
        #(false, stack_buf)
    and loop_rest may_need_space sexp stack_buf ~mach_maybe_esc_str = exclave_
      match sexp with
      | h :: t ->
        let #(may_need_space, stack_buf) =
          loop may_need_space h stack_buf ~mach_maybe_esc_str
        in
        loop_rest may_need_space t stack_buf ~mach_maybe_esc_str
      | [] -> Stack_buffer.add_char stack_buf ')'
    in
    let stack_buf = Stack_buffer.create () in
    let #(_, stack_buf) = loop false sexp stack_buf ~mach_maybe_esc_str in
    stack_buf
  ;;

  let bytes_of_stack_buffer__stack stack_buf = exclave_
    let len = Stack_buffer.length stack_buf in
    let bytes = Bytes.create__stack len in
    Stack_buffer.unsafe_blit_all stack_buf bytes;
    Bytes.unsafe_to_string bytes
  ;;

  let to_string_mach_internal__stack t ~mach_maybe_esc_str =
    match t with
    | Atom str -> exclave_ mach_maybe_esc_str str
    | sexp ->
      exclave_
      let stack_buf = to_stack_buffer_mach_internal__stack sexp ~mach_maybe_esc_str in
      let result = bytes_of_stack_buffer__stack stack_buf in
      result
  ;;

  let to_string_hum_internal__stack
    ?indent
    ?max_width
    sexp
    ~mach_maybe_esc_str
    ~maybe_globalize
    ~to_buffer_hum
    = exclave_
    match sexp with
    | Atom str
      when match index_of_newline str 0 with
           | None -> true
           | Some _ -> false -> mach_maybe_esc_str str
    | sexp ->
      let sexp = maybe_globalize sexp in
      let buf = buffer () in
      to_buffer_hum ~buf ?indent ?max_width sexp;
      bytes_of_buffer__stack buf
  ;;

  let bytes_of_buffer buf =
    let len = Buffer.length buf in
    let bytes = Bytes.create len in
    Buffer.blit buf 0 bytes 0 len;
    Bytes.unsafe_to_string bytes
  ;;

  let to_stack_buffer_mach_internal sexp ~mach_maybe_esc_str = exclave_
    let rec loop may_need_space sexp stack_buf ~mach_maybe_esc_str = exclave_
      match sexp with
      | Atom str ->
        let str' = mach_maybe_esc_str str in
        let new_may_need_space = str' == str in
        let stack_buf =
          if may_need_space && new_may_need_space
          then Stack_buffer.add_char stack_buf ' '
          else stack_buf
        in
        let stack_buf = Stack_buffer.add_string stack_buf str' in
        #(new_may_need_space, stack_buf)
      | List (h :: t) ->
        let stack_buf = Stack_buffer.add_char stack_buf '(' in
        let #(may_need_space, stack_buf) = loop false h stack_buf ~mach_maybe_esc_str in
        let stack_buf = loop_rest may_need_space t stack_buf ~mach_maybe_esc_str in
        #(false, stack_buf)
      | List [] ->
        let stack_buf = Stack_buffer.add_string stack_buf "()" in
        #(false, stack_buf)
    and loop_rest may_need_space sexp stack_buf ~mach_maybe_esc_str = exclave_
      match sexp with
      | h :: t ->
        let #(may_need_space, stack_buf) =
          loop may_need_space h stack_buf ~mach_maybe_esc_str
        in
        loop_rest may_need_space t stack_buf ~mach_maybe_esc_str
      | [] -> Stack_buffer.add_char stack_buf ')'
    in
    let stack_buf = Stack_buffer.create () in
    let #(_, stack_buf) = loop false sexp stack_buf ~mach_maybe_esc_str in
    stack_buf
  ;;

  let bytes_of_stack_buffer stack_buf =
    let len = Stack_buffer.length stack_buf in
    let bytes = Bytes.create len in
    Stack_buffer.unsafe_blit_all stack_buf bytes;
    Bytes.unsafe_to_string bytes
  ;;

  let to_string_mach_internal t ~mach_maybe_esc_str =
    match t with
    | Atom str -> mach_maybe_esc_str str
    | sexp ->
      let stack_buf = to_stack_buffer_mach_internal sexp ~mach_maybe_esc_str in
      let result = bytes_of_stack_buffer stack_buf in
      result
  ;;

  let to_string_hum_internal
    ?indent
    ?max_width
    sexp
    ~mach_maybe_esc_str
    ~maybe_globalize
    ~to_buffer_hum
    =
    match sexp with
    | Atom str
      when match index_of_newline str 0 with
           | None -> true
           | Some _ -> false -> mach_maybe_esc_str str
    | sexp ->
      let sexp = maybe_globalize sexp in
      let buf = buffer () in
      to_buffer_hum ~buf ?indent ?max_width sexp;
      bytes_of_buffer buf
  ;;

  [@@@end]

  module Make_pretty_printing (Helpers : Pretty_printing_helpers) :
    Pretty_printing with type output := string = struct
    include Helpers

    let to_buffer_hum ~buf ?(indent = Dynamic.get default_indent) ?max_width sexp =
      let ppf = Format.formatter_of_buffer buf in
      let () =
        match max_width with
        | Some width -> Format.pp_set_margin ppf width
        | None -> ()
      in
      Format.fprintf ppf "%a@?" (pp_hum_indent indent) sexp
    ;;

    let to_buffer_mach ~buf sexp = to_buffer_mach_internal ~buf sexp ~mach_maybe_esc_str
    let to_buffer = to_buffer_mach

    let to_buffer_gen ~buf ~add_char ~add_string sexp =
      let rec loop may_need_space = function
        | Atom str ->
          let str' = mach_maybe_esc_str str in
          let new_may_need_space = str' == str in
          if may_need_space && new_may_need_space then add_char buf ' ';
          add_string buf str';
          new_may_need_space
        | List (h :: t) ->
          add_char buf '(';
          let may_need_space = loop false h in
          loop_rest may_need_space t;
          false
        | List [] ->
          add_string buf "()";
          false
      and loop_rest may_need_space = function
        | h :: t ->
          let may_need_space = loop may_need_space h in
          loop_rest may_need_space t
        | [] -> add_char buf ')'
      in
      ignore (loop false sexp)
    ;;

    (* String conversions *)

    let maybe_globalize sexp = sexp

    let to_string_hum ?indent ?max_width sexp =
      to_string_hum_internal
        ?indent
        ?max_width
        sexp
        ~mach_maybe_esc_str
        ~maybe_globalize
        ~to_buffer_hum
    ;;

    let to_string_mach sexp = to_string_mach_internal sexp ~mach_maybe_esc_str
    let to_string = to_string_mach

    module Pretty_printing_helpers_private = Helpers
  end

  (* Escaping of strings used as atoms in S-expressions *)

  module Printing_helpers = struct
    let must_escape str =
      let len = String.length str in
      len = 0
      ||
      let rec loop str ix =
        match str.[ix] with
        | '"' | '(' | ')' | ';' | '\\' -> true
        | '|' ->
          ix > 0
          &&
          let next = ix - 1 in
          Char.equal str.[next] '#' || loop str next
        | '#' ->
          ix > 0
          &&
          let next = ix - 1 in
          Char.equal str.[next] '|' || loop str next
        | '\000' .. '\032' | '\127' .. '\255' -> true
        | _ -> ix > 0 && loop str (ix - 1)
      in
      loop str (len - 1)
    ;;

    let length_of_escaped_string s =
      let n = stack_ (ref 0) in
      for i = 0 to String.length s - 1 do
        n
        := !n
           +
           match String.unsafe_get s i with
           | '\"' | '\\' | '\n' | '\t' | '\r' | '\b' -> 2
           | ' ' .. '~' -> 1
           | _ -> 4
      done;
      !n
    ;;

    let escaped_bytes s bytes =
      let n = stack_ (ref 0) in
      n := 0;
      for i = 0 to String.length s - 1 do
        (match String.unsafe_get s i with
         | ('\"' | '\\') as c ->
           Bytes.unsafe_set bytes !n '\\';
           incr n;
           Bytes.unsafe_set bytes !n c
         | '\n' ->
           Bytes.unsafe_set bytes !n '\\';
           incr n;
           Bytes.unsafe_set bytes !n 'n'
         | '\t' ->
           Bytes.unsafe_set bytes !n '\\';
           incr n;
           Bytes.unsafe_set bytes !n 't'
         | '\r' ->
           Bytes.unsafe_set bytes !n '\\';
           incr n;
           Bytes.unsafe_set bytes !n 'r'
         | '\b' ->
           Bytes.unsafe_set bytes !n '\\';
           incr n;
           Bytes.unsafe_set bytes !n 'b'
         | ' ' .. '~' as c -> Bytes.unsafe_set bytes !n c
         | c ->
           let a = Char.code c in
           Bytes.unsafe_set bytes !n '\\';
           incr n;
           Bytes.unsafe_set bytes !n (Char.chr (48 + (a / 100)));
           incr n;
           Bytes.unsafe_set bytes !n (Char.chr (48 + (a / 10 mod 10)));
           incr n;
           Bytes.unsafe_set bytes !n (Char.chr (48 + (a mod 10))));
        incr n
      done
    ;;

    [@@@expand_inline
      [%%template
      [@@@alloc.default a @ m = (heap_global, stack_local)]

      let escaped s =
        (let length_of_escaped_string = length_of_escaped_string s in
         if length_of_escaped_string = String.length s
         then s
         else (
           let bytes = (Bytes.create [@alloc a]) length_of_escaped_string in
           escaped_bytes s bytes;
           Bytes.unsafe_to_string bytes))
        [@exclave_if_stack a]
      ;;

      let esc_str str =
        (let estr = (escaped [@alloc a]) str in
         let elen = String.length estr in
         let res = (Bytes.create [@alloc a]) (elen + 2) in
         Bytes.unsafe_blit_string ~src:estr ~src_pos:0 ~dst:res ~dst_pos:1 ~len:elen;
         Bytes.unsafe_set res 0 '"';
         Bytes.unsafe_set res (elen + 1) '"';
         Bytes.unsafe_to_string res)
        [@exclave_if_stack a]
      ;;

      let mach_maybe_esc_str str =
        (if must_escape str then (esc_str [@alloc a]) str else str) [@exclave_if_stack a]
      ;;

      let to_string_mach sexp =
        (to_string_mach_internal [@alloc a])
          sexp
          ~mach_maybe_esc_str:(mach_maybe_esc_str [@alloc a]) [@exclave_if_stack a]
      ;;

      let to_string = (to_string_mach [@alloc a])]]

    let escaped s =
      let length_of_escaped_string = length_of_escaped_string s in
      if length_of_escaped_string = String.length s
      then s
      else (
        let bytes = Bytes.create length_of_escaped_string in
        escaped_bytes s bytes;
        Bytes.unsafe_to_string bytes)
    ;;

    let esc_str str =
      let estr = escaped str in
      let elen = String.length estr in
      let res = Bytes.create (elen + 2) in
      Bytes.unsafe_blit_string ~src:estr ~src_pos:0 ~dst:res ~dst_pos:1 ~len:elen;
      Bytes.unsafe_set res 0 '"';
      Bytes.unsafe_set res (elen + 1) '"';
      Bytes.unsafe_to_string res
    ;;

    let mach_maybe_esc_str str = if must_escape str then esc_str str else str
    let to_string_mach sexp = to_string_mach_internal sexp ~mach_maybe_esc_str
    let to_string = to_string_mach

    let escaped__stack s = exclave_
      let length_of_escaped_string = length_of_escaped_string s in
      if length_of_escaped_string = String.length s
      then s
      else (
        let bytes = Bytes.create__stack length_of_escaped_string in
        escaped_bytes s bytes;
        Bytes.unsafe_to_string bytes)
    ;;

    let esc_str__stack str = exclave_
      let estr = escaped__stack str in
      let elen = String.length estr in
      let res = Bytes.create__stack (elen + 2) in
      Bytes.unsafe_blit_string ~src:estr ~src_pos:0 ~dst:res ~dst_pos:1 ~len:elen;
      Bytes.unsafe_set res 0 '"';
      Bytes.unsafe_set res (elen + 1) '"';
      Bytes.unsafe_to_string res
    ;;

    let mach_maybe_esc_str__stack str = exclave_
      if must_escape str then esc_str__stack str else str
    ;;

    let to_string_mach__stack sexp = exclave_
      to_string_mach_internal__stack sexp ~mach_maybe_esc_str:mach_maybe_esc_str__stack
    ;;

    let to_string__stack = to_string_mach__stack

    [@@@end]

    let get_substring str index end_pos_opt =
      let end_pos =
        match end_pos_opt with
        | None -> String.length str
        | Some end_pos -> end_pos
      in
      String.sub str ~pos:index ~len:(end_pos - index)
    ;;

    let is_one_line str =
      match index_of_newline str 0 with
      | None -> true
      | Some index -> index + 1 = String.length str
    ;;

    let pp_hum_maybe_esc_str ppf str =
      if not (must_escape str)
      then pp_print_string ppf str
      else if is_one_line str
      then pp_print_string ppf (esc_str str)
      else (
        let rec loop index =
          let next_newline = index_of_newline str index in
          let next_line = get_substring str index next_newline in
          pp_print_string ppf (escaped next_line);
          match next_newline with
          | None -> ()
          | Some newline_index ->
            pp_print_string ppf "\\";
            pp_force_newline ppf ();
            pp_print_string ppf "\\n";
            loop (newline_index + 1)
        in
        pp_open_box ppf 0;
        (* the leading space is to line up the lines *)
        pp_print_string ppf " \"";
        loop 0;
        pp_print_string ppf "\"";
        pp_close_box ppf ())
    ;;

    (* Output of S-expressions to formatters *)

    let rec pp_hum_indent indent ppf = function
      | Atom str -> pp_hum_maybe_esc_str ppf str
      | List (h :: t) ->
        pp_open_box ppf indent;
        pp_print_string ppf "(";
        pp_hum_indent indent ppf h;
        pp_hum_rest indent ppf t
      | List [] -> pp_print_string ppf "()"

    and pp_hum_rest indent ppf = function
      | h :: t ->
        pp_print_space ppf ();
        pp_hum_indent indent ppf h;
        pp_hum_rest indent ppf t
      | [] ->
        pp_print_string ppf ")";
        pp_close_box ppf ()
    ;;

    let rec pp_mach_internal may_need_space ppf = function
      | Atom str ->
        let str' = mach_maybe_esc_str str in
        let new_may_need_space = str' == str in
        if may_need_space && new_may_need_space then pp_print_string ppf " ";
        pp_print_string ppf str';
        new_may_need_space
      | List (h :: t) ->
        pp_print_string ppf "(";
        let may_need_space = pp_mach_internal false ppf h in
        pp_mach_rest may_need_space ppf t;
        false
      | List [] ->
        pp_print_string ppf "()";
        false

    and pp_mach_rest may_need_space ppf = function
      | h :: t ->
        let may_need_space = pp_mach_internal may_need_space ppf h in
        pp_mach_rest may_need_space ppf t
      | [] -> pp_print_string ppf ")"
    ;;

    let pp_hum ppf sexp = pp_hum_indent (Dynamic.get default_indent) ppf sexp
    let pp_mach ppf sexp = ignore (pp_mach_internal false ppf sexp)
    let pp = pp_mach
  end

  (* Sexp size *)

  let rec size_loop ((v, c) as acc) = function
    | Atom str -> v + 1, c + String.length str
    | List lst -> List.fold_left lst ~init:acc ~f:size_loop
  ;;

  let size sexp = size_loop (0, 0) sexp

  (* Buffer conversions *)

  include Make_pretty_printing (Printing_helpers)
  include Printing_helpers
end

include Printing

let globalize = Globalize.globalize

let to_string_hum__stack ?indent ?max_width sexp = exclave_
  to_string_hum_internal__stack
    ?indent
    ?max_width
    sexp
    ~mach_maybe_esc_str:mach_maybe_esc_str__stack
    ~maybe_globalize:Globalize.maybe_globalize
    ~to_buffer_hum
;;

let of_float_style = Dynamic.make (`No_underscores : [ `Underscores | `No_underscores ])
let of_int_style = Dynamic.make (`No_underscores : [ `Underscores | `No_underscores ])

module Private = struct
  include Printing
end

let message name fields =
  let rec conv_fields = function
    | [] -> []
    | (fname, fsexp) :: rest ->
      (match fname with
       | "" -> fsexp :: conv_fields rest
       | _ -> List [ Atom fname; fsexp ] :: conv_fields rest)
  in
  List (Atom name :: conv_fields fields)
;;
