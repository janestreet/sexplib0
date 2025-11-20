open! Core
open Expect_test_helpers_base
open Sexplib0

let stack_allocated_sexp () : Sexp.t = exclave_
  let make =
    Sys.opaque_identity (fun string : Sexp.t -> exclave_ stack_ List [ Atom string ])
  in
  let string = (Sys.opaque_identity String.make) 5 'a' in
  make string
;;

let heap_allocated_sexp () : Sexp.t = Sexp.globalize (stack_allocated_sexp ()) [@nontail]
let statically_allocated_sexp () : Sexp.t = List [ Atom "aaaaa" ]

let%expect_test ("make sure a stack-allocated sexp doesn't break [maybe_globalize] and \
                  that it allocates more than a heap-allocated sexp" [@tags "no-js"])
  =
  let measure_to_string_hum ~(here : [%call_pos]) sexp =
    let f () =
      ignore (Sys.opaque_identity ((Sexp.to_string_hum [@alloc stack]) sexp) : string)
    in
    let #( ()
         , ({ major_words_allocated; minor_words_allocated } :
             Gc.For_testing.Allocation_report.t) )
      =
      Gc.For_testing.measure_allocation f
    in
    require_equal ~here (module Int) major_words_allocated 0;
    minor_words_allocated
  in
  let minor_words_stack = measure_to_string_hum (stack_allocated_sexp ()) in
  let minor_words_heap = measure_to_string_hum (heap_allocated_sexp ()) in
  require (minor_words_stack > minor_words_heap);
  let minor_words_static = measure_to_string_hum (statically_allocated_sexp ()) in
  require (minor_words_stack > minor_words_static);
  [%expect {| |}]
;;
