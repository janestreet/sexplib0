open! Base
open Expect_test_helpers_base
open Sexplib0

let () = sexp_style := Sexp_style.simple_pretty

module type S = sig
  type t [@@deriving equal, sexp]
end

let test (type a) (module M : S with type t = a) string =
  let sexp = Parsexp.Single.parse_string_exn string in
  let result = Or_error.try_with (fun () -> M.t_of_sexp sexp) in
  print_s [%sexp (result : M.t Or_error.t)]
;;

(** All possible ways of constructing [Sexp_conv_record.Layout_witness.t] values, together
    with a uniform interface for manipulating them. The tests below use this uniform
    interface to write tests that try all possible combinations of layouts (i.e., both
    [Any] and [Value]). *)
module Layout_impl = struct
  type (_, _) t =
    | Value : ('a, 'a) t
    | Any : ('a, unit -> 'a) t

  type _ packed = T : ('a, _) t -> 'a packed [@@unboxed]

  let all = [ T Value; T Any ]
  let all_pairs () = List.cartesian_product all all

  let value : type a b. (a, b) t -> b -> a = function
    | Value -> fun x -> x
    | Any -> fun f -> f ()
  ;;
end

let field
  : type a b c conv.
    name:string
    -> kind:(a, conv) Sexp_conv_record.Kind.t
    -> impl:(a, b) Layout_impl.t
    -> conv:conv
    -> rest:c Sexp_conv_record.Fields.t
    -> (b * c) Sexp_conv_record.Fields.t
  =
  fun ~name ~kind ~impl ~conv ~rest ->
  let const x () = x in
  let thunk f x = const (f x) in
  match impl with
  | Value -> Field { name; kind; layout = Value; conv; rest }
  | Any ->
    (match kind with
     | Default f ->
       Field { name; kind = Default (thunk f); layout = Value; conv = thunk conv; rest }
     | Omit_nil ->
       Field { name; kind = Omit_nil; layout = Value; conv = thunk conv; rest }
     | Required ->
       Field { name; kind = Required; layout = Value; conv = thunk conv; rest }
     | Sexp_array | Sexp_bool | Sexp_list | Sexp_option ->
       failwith "[array], [bool], [list], and [option] all have layout [value]")
;;

let%expect_test "simple record" =
  List.iter (Layout_impl.all_pairs ()) ~f:(fun (T impl1, T impl2) ->
    let module M = struct
      type t =
        { x : int
        ; y : int
        }
      [@@deriving equal, sexp_of]

      let t_of_sexp sexp =
        Sexp_conv_record.record_of_sexp
          sexp
          ~caller:"M.t"
          ~fields:
            (field
               ~name:"x"
               ~kind:Required
               ~impl:impl1
               ~conv:int_of_sexp
               ~rest:
                 (field
                    ~name:"y"
                    ~kind:Required
                    ~impl:impl2
                    ~conv:int_of_sexp
                    ~rest:Empty))
          ~index_of_field:(function
            | "x" -> 0
            | "y" -> 1
            | _ -> -1)
          ~allow_extra_fields:false
          ~create:(fun (x, (y, ())) ->
            { x = Layout_impl.value impl1 x; y = Layout_impl.value impl2 y })
      ;;
    end
    in
    let test = test (module M) in
    (* in order *)
    test "((x 1) (y 2))";
    [%expect {| (Ok ((x 1) (y 2))) |}];
    (* reverse order *)
    test "((y 2) (x 1))";
    [%expect {| (Ok ((x 1) (y 2))) |}];
    (* duplicate fields *)
    test "((x 1) (x 2) (y 3) (y 4))";
    [%expect
      {|
      (Error
       (Of_sexp_error
        "M.t_of_sexp: duplicate fields: x y"
        (invalid_sexp ((x 1) (x 2) (y 3) (y 4)))))
      |}];
    (* extra fields *)
    test "((a 1) (b 2) (c 3))";
    [%expect
      {|
      (Error
       (Of_sexp_error
        "M.t_of_sexp: extra fields found while some fields missing; extra fields: a b c; missing fields: x y"
        (invalid_sexp ((a 1) (b 2) (c 3)))))
      |}];
    (* missing field *)
    test "((x 1))";
    [%expect
      {|
      (Error
       (Of_sexp_error "M.t_of_sexp: missing fields: y" (invalid_sexp ((x 1)))))
      |}];
    (* other missing field *)
    test "((y 2))";
    [%expect
      {|
      (Error
       (Of_sexp_error "M.t_of_sexp: missing fields: x" (invalid_sexp ((y 2)))))
      |}];
    (* multiple missing fields *)
    test "()";
    [%expect
      {| (Error (Of_sexp_error "M.t_of_sexp: missing fields: x y" (invalid_sexp ()))) |}];
    ())
;;

let%expect_test "record with extra fields" =
  List.iter (Layout_impl.all_pairs ()) ~f:(fun (T impl1, T impl2) ->
    let module M = struct
      type t =
        { x : int
        ; y : int
        }
      [@@deriving equal, sexp_of]

      let t_of_sexp =
        Sexp_conv_record.record_of_sexp
          ~caller:"M.t"
          ~fields:
            (field
               ~name:"x"
               ~kind:Required
               ~impl:impl1
               ~conv:int_of_sexp
               ~rest:
                 (field
                    ~name:"y"
                    ~kind:Required
                    ~impl:impl2
                    ~conv:int_of_sexp
                    ~rest:Empty))
          ~index_of_field:(function
            | "x" -> 0
            | "y" -> 1
            | _ -> -1)
          ~allow_extra_fields:true
          ~create:(fun (x, (y, ())) ->
            { x = Layout_impl.value impl1 x; y = Layout_impl.value impl2 y })
      ;;
    end
    in
    let test = test (module M) in
    (* in order *)
    test "((x 1) (y 2))";
    [%expect {| (Ok ((x 1) (y 2))) |}];
    (* reversed order *)
    test "((y 2) (x 1))";
    [%expect {| (Ok ((x 1) (y 2))) |}];
    (* extra field *)
    test "((x 1) (y 2) (z 3))";
    [%expect {| (Ok ((x 1) (y 2))) |}];
    (* missing field *)
    test "((x 1))";
    [%expect
      {|
      (Error
       (Of_sexp_error "M.t_of_sexp: missing fields: y" (invalid_sexp ((x 1)))))
      |}];
    (* other missing field *)
    test "((y 2))";
    [%expect
      {|
      (Error
       (Of_sexp_error "M.t_of_sexp: missing fields: x" (invalid_sexp ((y 2)))))
      |}];
    (* multiple missing fields *)
    test "()";
    [%expect
      {| (Error (Of_sexp_error "M.t_of_sexp: missing fields: x y" (invalid_sexp ()))) |}];
    ())
;;

let%expect_test "record with defaults" =
  List.iter (Layout_impl.all_pairs ()) ~f:(fun (T impl1, T impl2) ->
    let module M = struct
      type t =
        { x : int
        ; y : int
        }
      [@@deriving equal, sexp_of]

      let t_of_sexp =
        Sexp_conv_record.record_of_sexp
          ~caller:"M.t"
          ~fields:
            (field
               ~name:"x"
               ~kind:(Default (fun () -> 0))
               ~impl:impl1
               ~conv:int_of_sexp
               ~rest:
                 (field
                    ~name:"y"
                    ~kind:(Default (fun () -> 0))
                    ~impl:impl2
                    ~conv:int_of_sexp
                    ~rest:Empty))
          ~index_of_field:(function
            | "x" -> 0
            | "y" -> 1
            | _ -> -1)
          ~allow_extra_fields:false
          ~create:(fun (x, (y, ())) ->
            { x = Layout_impl.value impl1 x; y = Layout_impl.value impl2 y })
      ;;
    end
    in
    let test = test (module M) in
    (* in order *)
    test "((x 1) (y 2))";
    [%expect {| (Ok ((x 1) (y 2))) |}];
    (* reverse order *)
    test "((y 2) (x 1))";
    [%expect {| (Ok ((x 1) (y 2))) |}];
    (* extra field *)
    test "((x 1) (y 2) (z 3))";
    [%expect
      {|
      (Error
       (Of_sexp_error
        "M.t_of_sexp: extra fields: z"
        (invalid_sexp ((x 1) (y 2) (z 3)))))
      |}];
    (* missing field *)
    test "((x 1))";
    [%expect {| (Ok ((x 1) (y 0))) |}];
    (* other missing field *)
    test "((y 2))";
    [%expect {| (Ok ((x 0) (y 2))) |}];
    (* multiple missing fields *)
    test "()";
    [%expect {| (Ok ((x 0) (y 0))) |}];
    ())
;;

let%expect_test "record with omit nil" =
  List.iter (Layout_impl.all_pairs ()) ~f:(fun (T impl1, T impl2) ->
    let module M = struct
      type t =
        { a : int option
        ; b : int list
        }
      [@@deriving equal, sexp_of]

      let t_of_sexp =
        Sexp_conv_record.record_of_sexp
          ~caller:"M.t"
          ~fields:
            (field
               ~name:"a"
               ~kind:Omit_nil
               ~impl:impl1
               ~conv:(option_of_sexp int_of_sexp)
               ~rest:
                 (field
                    ~name:"b"
                    ~kind:Omit_nil
                    ~impl:impl2
                    ~conv:(list_of_sexp int_of_sexp)
                    ~rest:Empty))
          ~index_of_field:(function
            | "a" -> 0
            | "b" -> 1
            | _ -> -1)
          ~allow_extra_fields:false
          ~create:(fun (a, (b, ())) ->
            { a = Layout_impl.value impl1 a; b = Layout_impl.value impl2 b })
      ;;
    end
    in
    let test = test (module M) in
    (* in order *)
    test "((a (1)) (b (2 3)))";
    [%expect {| (Ok ((a (1)) (b (2 3)))) |}];
    (* reverse order *)
    test "((b ()) (a ()))";
    [%expect {| (Ok ((a ()) (b ()))) |}];
    (* extra field *)
    test "((a (1)) (b (2 3)) (z ()))";
    [%expect
      {|
      (Error
       (Of_sexp_error
        "M.t_of_sexp: extra fields: z"
        (invalid_sexp ((a (1)) (b (2 3)) (z ())))))
      |}];
    (* missing field *)
    test "((a (1)))";
    [%expect {| (Ok ((a (1)) (b ()))) |}];
    (* other missing field *)
    test "((b (2 3)))";
    [%expect {| (Ok ((a ()) (b (2 3)))) |}];
    (* multiple missing fields *)
    test "()";
    [%expect {| (Ok ((a ()) (b ()))) |}];
    ())
;;

let%expect_test "record with sexp types" =
  let module M = struct
    type t =
      { a : int option
      ; b : int list
      ; c : int array
      ; d : bool
      }
    [@@deriving equal, sexp_of]

    let t_of_sexp =
      Sexp_conv_record.record_of_sexp
        ~caller:"M.t"
        ~fields:
          (Field
             { name = "a"
             ; kind = Sexp_option
             ; layout = Value
             ; conv = int_of_sexp
             ; rest =
                 Field
                   { name = "b"
                   ; kind = Sexp_list
                   ; layout = Value
                   ; conv = int_of_sexp
                   ; rest =
                       Field
                         { name = "c"
                         ; kind = Sexp_array
                         ; layout = Value
                         ; conv = int_of_sexp
                         ; rest =
                             Field
                               { name = "d"
                               ; kind = Sexp_bool
                               ; layout = Value
                               ; conv = ()
                               ; rest = Empty
                               }
                         }
                   }
             })
        ~index_of_field:(function
          | "a" -> 0
          | "b" -> 1
          | "c" -> 2
          | "d" -> 3
          | _ -> -1)
        ~allow_extra_fields:false
        ~create:(fun (a, (b, (c, (d, ())))) -> { a; b; c; d })
    ;;
  end
  in
  let test = test (module M) in
  (* in order *)
  test "((a 1) (b (2 3)) (c (4 5)) (d))";
  [%expect {| (Ok ((a (1)) (b (2 3)) (c (4 5)) (d true))) |}];
  (* reverse order *)
  test "((d) (c ()) (b ()) (a 1))";
  [%expect {| (Ok ((a (1)) (b ()) (c ()) (d true))) |}];
  (* missing field d *)
  test "((a 1) (b (2 3)) (c (4 5)))";
  [%expect {| (Ok ((a (1)) (b (2 3)) (c (4 5)) (d false))) |}];
  (* missing field c *)
  test "((a 1) (b (2 3)) (d))";
  [%expect {| (Ok ((a (1)) (b (2 3)) (c ()) (d true))) |}];
  (* missing field b *)
  test "((a 1) (c (2 3)) (d))";
  [%expect {| (Ok ((a (1)) (b ()) (c (2 3)) (d true))) |}];
  (* missing field a *)
  test "((b (1 2)) (c (3 4)) (d))";
  [%expect {| (Ok ((a ()) (b (1 2)) (c (3 4)) (d true))) |}];
  (* extra field *)
  test "((a 1) (b (2 3)) (c (4 5)) (d) (e (6 7)))";
  [%expect
    {|
    (Error
     (Of_sexp_error
      "M.t_of_sexp: extra fields: e"
      (invalid_sexp ((a 1) (b (2 3)) (c (4 5)) (d) (e (6 7))))))
    |}];
  (* all fields missing *)
  test "()";
  [%expect {| (Ok ((a ()) (b ()) (c ()) (d false))) |}];
  ()
;;

let%expect_test "record with polymorphic fields" =
  let module M = struct
    type t =
      { a : 'a. 'a list
      ; b : 'a 'b. ('a, 'b) Result.t option
      }
    [@@deriving sexp_of]

    let equal = Poly.equal

    type a = { a : 'a. 'a list } [@@unboxed]
    type b = { b : 'a 'b. ('a, 'b) Result.t option } [@@unboxed]
  end
  in
  (* Unlike other tests, we write the skeleton of module [M] before performing the nested
     iteration over layout impls. This is for a silly OCaml value-restriction-flavored
     reason, not for a good reason: the below [all_pairs ()] introduces a weak type
     variable. That weak type variable ends up needs to unify with the local type [M.a]
     (and another one with [M.b]), so it's necessary for these local types to be defined
     first so this unification is not considered to make them "escape their scope".
  *)
  List.iter (Layout_impl.all_pairs ()) ~f:(fun (T impl1, T impl2) ->
    let module M = struct
      include M

      let t_of_sexp =
        let caller = "M.t" in
        Sexp_conv_record.record_of_sexp
          ~caller
          ~fields:
            (field
               ~name:"a"
               ~kind:Required
               ~impl:impl1
               ~conv:(fun sexp ->
                 { a =
                     list_of_sexp (Sexplib.Conv_error.record_poly_field_value caller) sexp
                 })
               ~rest:
                 (field
                    ~name:"b"
                    ~kind:Required
                    ~impl:impl2
                    ~conv:(fun sexp ->
                      { b =
                          Option.t_of_sexp
                            (Result.t_of_sexp
                               (Sexplib.Conv_error.record_poly_field_value caller)
                               (Sexplib.Conv_error.record_poly_field_value caller))
                            sexp
                      })
                    ~rest:Empty))
          ~index_of_field:(function
            | "a" -> 0
            | "b" -> 1
            | _ -> -1)
          ~allow_extra_fields:false
          ~create:(fun (a, (b, ())) ->
            let { a } = Layout_impl.value impl1 a
            and { b } = Layout_impl.value impl2 b in
            { a; b })
      ;;
    end
    in
    let test = test (module M) in
    (* in order *)
    test "((a ()) (b ()))";
    [%expect {| (Ok ((a ()) (b ()))) |}];
    (* reverse order *)
    test "((b ()) (a ()))";
    [%expect {| (Ok ((a ()) (b ()))) |}];
    (* attempt to deserialize paramter to [a] *)
    test "((a (_)) (b ()))";
    [%expect
      {|
      (Error
       (Of_sexp_error
        "M.t_of_sexp: cannot convert values of types resulting from polymorphic record fields"
        (invalid_sexp _)))
      |}];
    (* attempt to deserialize first parameter to [b] *)
    test "((a ()) (b ((Ok _))))";
    [%expect
      {|
      (Error
       (Of_sexp_error
        "M.t_of_sexp: cannot convert values of types resulting from polymorphic record fields"
        (invalid_sexp _)))
      |}];
    (* attempt to deserialize second parameter to [b] *)
    test "((a ()) (b ((Error _))))";
    [%expect
      {|
      (Error
       (Of_sexp_error
        "M.t_of_sexp: cannot convert values of types resulting from polymorphic record fields"
        (invalid_sexp _)))
      |}];
    (* multiple missing fields *)
    test "()";
    [%expect
      {| (Error (Of_sexp_error "M.t_of_sexp: missing fields: a b" (invalid_sexp ()))) |}];
    ())
;;

let%expect_test _ =
  let big_string = String.init 5_000_000 ~f:(fun i -> String.get (Int.to_string i) 0) in
  let sexp = [%sexp (big_string : string)] in
  let sexp_string =
    (* In an experimental compiler version, this would overflow the stack. *)
    Sexp.to_string sexp
  in
  print_endline (Int.to_string_hum (String.length sexp_string));
  [%expect {| 5_000_000 |}]
;;

(* Assert that the module types defined by sexplib0 are equivalent to those derived by
   ppx_sexp_conv. *)
module _ = struct
  module type S = sig
    type t [@@deriving sexp]
  end

  module type S1 = sig
    type 'a t [@@deriving sexp]
  end

  module type S2 = sig
    type ('a, 'b) t [@@deriving sexp]
  end

  module type S3 = sig
    type ('a, 'b, 'c) t [@@deriving sexp]
  end

  module type S_with_grammar = sig
    type t [@@deriving sexp, sexp_grammar]
  end

  module type S1_with_grammar = sig
    type 'a t [@@deriving sexp, sexp_grammar]
  end

  module type S2_with_grammar = sig
    type ('a, 'b) t [@@deriving sexp, sexp_grammar]
  end

  module type S3_with_grammar = sig
    type ('a, 'b, 'c) t [@@deriving sexp, sexp_grammar]
  end

  let (T : ((module Sexpable.S), (module S)) Type_equal.t) = T
  let (T : ((module Sexpable.S1), (module S1)) Type_equal.t) = T
  let (T : ((module Sexpable.S2), (module S2)) Type_equal.t) = T
  let (T : ((module Sexpable.S3), (module S3)) Type_equal.t) = T
  let (T : ((module Sexpable.S_with_grammar), (module S_with_grammar)) Type_equal.t) = T
  let (T : ((module Sexpable.S1_with_grammar), (module S1_with_grammar)) Type_equal.t) = T
  let (T : ((module Sexpable.S2_with_grammar), (module S2_with_grammar)) Type_equal.t) = T
  let (T : ((module Sexpable.S3_with_grammar), (module S3_with_grammar)) Type_equal.t) = T
end

module%test Illegal_chars = struct
  (* Test [sexp_of_char] against the naive implementation that dynamically creates the
     length-1 string. The focus of this test is on illegal representations: immediates
     that lie outside the range representable by [char] *)

  let[@inline never] sexp_of_char' (char : char) : Sexp.t =
    Atom ((String.make [@inlined never]) 1 char)
  ;;

  let test_at ~start ~num_tests =
    List.init num_tests ~f:(( + ) start)
    |> List.iter ~f:(fun (c : int) ->
      let c : char = Stdlib.Obj.magic c in
      Expect_test_helpers_base.require_equal
        (module Sexp)
        (sexp_of_char c)
        (sexp_of_char' c))
  ;;

  let%expect_test _ =
    test_at ~start:Int.min_value ~num_tests:0x10000;
    test_at ~start:(-0x10000) ~num_tests:0x20000;
    test_at ~start:(Int.max_value - 0xFFFF) ~num_tests:0x10000
  ;;
end
