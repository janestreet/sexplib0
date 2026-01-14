[@@@expand_inline
  [%%template
  module type Of_sexp = sig
    type t

    val t_of_sexp : Sexp.t -> t
  end

  [@@@alloc.default a @ m = (heap @ global, stack @ local)]

  module type Sexp_of = sig
    type t

    val sexp_of_t : t -> Sexp.t [@@alloc a @ m = (a @ m, heap @ global)]
  end

  module type S = sig
    type t

    include Of_sexp with type t := t
    include Sexp_of [@alloc a] with type t := t
  end

  [@@@kind.default ka = (value, any)]

  module type S1 = sig
    type 'a t

    val t_of_sexp : 'a. (Sexp.t -> 'a) -> Sexp.t -> 'a t

    val sexp_of_t : 'a. ('a -> Sexp.t) -> 'a t -> Sexp.t
    [@@alloc a @ m = (a @ m, heap @ global)]
  end

  [@@@kind.default kb = (value, any)]

  module type S2 = sig
    type ('a, 'b) t

    val t_of_sexp : 'a 'b. (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) t

    val sexp_of_t : 'a 'b. ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('a, 'b) t -> Sexp.t
    [@@alloc a @ m = (a @ m, heap @ global)]
  end

  [@@@kind.default kc = (value, any)]

  module type S3 = sig
    type ('a, 'b, 'c) t

    val t_of_sexp
      : 'a 'b 'c.
      (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c) -> Sexp.t -> ('a, 'b, 'c) t

    val sexp_of_t
      : 'a 'b 'c.
      ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('c -> Sexp.t) -> ('a, 'b, 'c) t -> Sexp.t
    [@@alloc a @ m = (a @ m, heap @ global)]
  end]]

module type Of_sexp = sig
  type t

  val t_of_sexp : Sexp.t -> t
end

include struct
  module type Sexp_of = sig
    type t

    val sexp_of_t : t -> Sexp.t
  end

  module type S = sig
    type t

    include Of_sexp with type t := t
    include Sexp_of with type t := t
  end

  include struct
    module type S1 = sig
      type 'a t

      val t_of_sexp : 'a. (Sexp.t -> 'a) -> Sexp.t -> 'a t
      val sexp_of_t : 'a. ('a -> Sexp.t) -> 'a t -> Sexp.t
    end

    include struct
      module type S2 = sig
        type ('a, 'b) t

        val t_of_sexp : 'a 'b. (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) t
        val sexp_of_t : 'a 'b. ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('a, 'b) t -> Sexp.t
      end

      include struct
        module type S3 = sig
          type ('a, 'b, 'c) t

          val t_of_sexp
            : 'a 'b 'c.
            (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c) -> Sexp.t -> ('a, 'b, 'c) t

          val sexp_of_t
            : 'a 'b 'c.
            ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('c -> Sexp.t) -> ('a, 'b, 'c) t -> Sexp.t
        end
      end [@@ocaml.doc " @inline "]

      include struct
        module type S3__value__value__any = sig
          type ('a, 'b, 'c) t

          val t_of_sexp
            : 'a 'b 'c.
            (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c) -> Sexp.t -> ('a, 'b, 'c) t

          val sexp_of_t
            : 'a 'b 'c.
            ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('c -> Sexp.t) -> ('a, 'b, 'c) t -> Sexp.t
        end
      end [@@ocaml.doc " @inline "]
    end [@@ocaml.doc " @inline "]

    include struct
      module type S2__value__any = sig
        type ('a, 'b) t

        val t_of_sexp : 'a 'b. (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) t
        val sexp_of_t : 'a 'b. ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('a, 'b) t -> Sexp.t
      end

      include struct
        module type S3__value__any__value = sig
          type ('a, 'b, 'c) t

          val t_of_sexp
            : 'a 'b 'c.
            (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c) -> Sexp.t -> ('a, 'b, 'c) t

          val sexp_of_t
            : 'a 'b 'c.
            ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('c -> Sexp.t) -> ('a, 'b, 'c) t -> Sexp.t
        end
      end [@@ocaml.doc " @inline "]

      include struct
        module type S3__value__any__any = sig
          type ('a, 'b, 'c) t

          val t_of_sexp
            : 'a 'b 'c.
            (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c) -> Sexp.t -> ('a, 'b, 'c) t

          val sexp_of_t
            : 'a 'b 'c.
            ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('c -> Sexp.t) -> ('a, 'b, 'c) t -> Sexp.t
        end
      end [@@ocaml.doc " @inline "]
    end [@@ocaml.doc " @inline "]
  end [@@ocaml.doc " @inline "]

  include struct
    module type S1__any = sig
      type 'a t

      val t_of_sexp : 'a. (Sexp.t -> 'a) -> Sexp.t -> 'a t
      val sexp_of_t : 'a. ('a -> Sexp.t) -> 'a t -> Sexp.t
    end

    include struct
      module type S2__any__value = sig
        type ('a, 'b) t

        val t_of_sexp : 'a 'b. (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) t
        val sexp_of_t : 'a 'b. ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('a, 'b) t -> Sexp.t
      end

      include struct
        module type S3__any__value__value = sig
          type ('a, 'b, 'c) t

          val t_of_sexp
            : 'a 'b 'c.
            (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c) -> Sexp.t -> ('a, 'b, 'c) t

          val sexp_of_t
            : 'a 'b 'c.
            ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('c -> Sexp.t) -> ('a, 'b, 'c) t -> Sexp.t
        end
      end [@@ocaml.doc " @inline "]

      include struct
        module type S3__any__value__any = sig
          type ('a, 'b, 'c) t

          val t_of_sexp
            : 'a 'b 'c.
            (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c) -> Sexp.t -> ('a, 'b, 'c) t

          val sexp_of_t
            : 'a 'b 'c.
            ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('c -> Sexp.t) -> ('a, 'b, 'c) t -> Sexp.t
        end
      end [@@ocaml.doc " @inline "]
    end [@@ocaml.doc " @inline "]

    include struct
      module type S2__any__any = sig
        type ('a, 'b) t

        val t_of_sexp : 'a 'b. (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) t
        val sexp_of_t : 'a 'b. ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('a, 'b) t -> Sexp.t
      end

      include struct
        module type S3__any__any__value = sig
          type ('a, 'b, 'c) t

          val t_of_sexp
            : 'a 'b 'c.
            (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c) -> Sexp.t -> ('a, 'b, 'c) t

          val sexp_of_t
            : 'a 'b 'c.
            ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('c -> Sexp.t) -> ('a, 'b, 'c) t -> Sexp.t
        end
      end [@@ocaml.doc " @inline "]

      include struct
        module type S3__any__any__any = sig
          type ('a, 'b, 'c) t

          val t_of_sexp
            : 'a 'b 'c.
            (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c) -> Sexp.t -> ('a, 'b, 'c) t

          val sexp_of_t
            : 'a 'b 'c.
            ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('c -> Sexp.t) -> ('a, 'b, 'c) t -> Sexp.t
        end
      end [@@ocaml.doc " @inline "]
    end [@@ocaml.doc " @inline "]
  end [@@ocaml.doc " @inline "]
end [@@ocaml.doc " @inline "]

include struct
  module type Sexp_of__stack = sig
    type t

    [@@@ocaml.text "/*"]

    val sexp_of_t__stack : t -> Sexp.t

    [@@@ocaml.text "/*"]

    val sexp_of_t : t -> Sexp.t
  end

  module type S__stack = sig
    type t

    include Of_sexp with type t := t
    include Sexp_of__stack with type t := t
  end

  include struct
    module type S1__stack = sig
      type 'a t

      val t_of_sexp : 'a. (Sexp.t -> 'a) -> Sexp.t -> 'a t

      [@@@ocaml.text "/*"]

      val sexp_of_t__stack : 'a. ('a -> Sexp.t) -> 'a t -> Sexp.t

      [@@@ocaml.text "/*"]

      val sexp_of_t : 'a. ('a -> Sexp.t) -> 'a t -> Sexp.t
    end

    include struct
      module type S2__stack = sig
        type ('a, 'b) t

        val t_of_sexp : 'a 'b. (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) t

        [@@@ocaml.text "/*"]

        val sexp_of_t__stack
          : 'a 'b.
          ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('a, 'b) t -> Sexp.t

        [@@@ocaml.text "/*"]

        val sexp_of_t : 'a 'b. ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('a, 'b) t -> Sexp.t
      end

      include struct
        module type S3__stack = sig
          type ('a, 'b, 'c) t

          val t_of_sexp
            : 'a 'b 'c.
            (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c) -> Sexp.t -> ('a, 'b, 'c) t

          [@@@ocaml.text "/*"]

          val sexp_of_t__stack
            : 'a 'b 'c.
            ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('c -> Sexp.t) -> ('a, 'b, 'c) t -> Sexp.t

          [@@@ocaml.text "/*"]

          val sexp_of_t
            : 'a 'b 'c.
            ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('c -> Sexp.t) -> ('a, 'b, 'c) t -> Sexp.t
        end
      end [@@ocaml.doc " @inline "]

      include struct
        module type S3__value__value__any__stack = sig
          type ('a, 'b, 'c) t

          val t_of_sexp
            : 'a 'b 'c.
            (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c) -> Sexp.t -> ('a, 'b, 'c) t

          [@@@ocaml.text "/*"]

          val sexp_of_t__stack
            : 'a 'b 'c.
            ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('c -> Sexp.t) -> ('a, 'b, 'c) t -> Sexp.t

          [@@@ocaml.text "/*"]

          val sexp_of_t
            : 'a 'b 'c.
            ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('c -> Sexp.t) -> ('a, 'b, 'c) t -> Sexp.t
        end
      end [@@ocaml.doc " @inline "]
    end [@@ocaml.doc " @inline "]

    include struct
      module type S2__value__any__stack = sig
        type ('a, 'b) t

        val t_of_sexp : 'a 'b. (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) t

        [@@@ocaml.text "/*"]

        val sexp_of_t__stack
          : 'a 'b.
          ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('a, 'b) t -> Sexp.t

        [@@@ocaml.text "/*"]

        val sexp_of_t : 'a 'b. ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('a, 'b) t -> Sexp.t
      end

      include struct
        module type S3__value__any__value__stack = sig
          type ('a, 'b, 'c) t

          val t_of_sexp
            : 'a 'b 'c.
            (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c) -> Sexp.t -> ('a, 'b, 'c) t

          [@@@ocaml.text "/*"]

          val sexp_of_t__stack
            : 'a 'b 'c.
            ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('c -> Sexp.t) -> ('a, 'b, 'c) t -> Sexp.t

          [@@@ocaml.text "/*"]

          val sexp_of_t
            : 'a 'b 'c.
            ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('c -> Sexp.t) -> ('a, 'b, 'c) t -> Sexp.t
        end
      end [@@ocaml.doc " @inline "]

      include struct
        module type S3__value__any__any__stack = sig
          type ('a, 'b, 'c) t

          val t_of_sexp
            : 'a 'b 'c.
            (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c) -> Sexp.t -> ('a, 'b, 'c) t

          [@@@ocaml.text "/*"]

          val sexp_of_t__stack
            : 'a 'b 'c.
            ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('c -> Sexp.t) -> ('a, 'b, 'c) t -> Sexp.t

          [@@@ocaml.text "/*"]

          val sexp_of_t
            : 'a 'b 'c.
            ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('c -> Sexp.t) -> ('a, 'b, 'c) t -> Sexp.t
        end
      end [@@ocaml.doc " @inline "]
    end [@@ocaml.doc " @inline "]
  end [@@ocaml.doc " @inline "]

  include struct
    module type S1__any__stack = sig
      type 'a t

      val t_of_sexp : 'a. (Sexp.t -> 'a) -> Sexp.t -> 'a t

      [@@@ocaml.text "/*"]

      val sexp_of_t__stack : 'a. ('a -> Sexp.t) -> 'a t -> Sexp.t

      [@@@ocaml.text "/*"]

      val sexp_of_t : 'a. ('a -> Sexp.t) -> 'a t -> Sexp.t
    end

    include struct
      module type S2__any__value__stack = sig
        type ('a, 'b) t

        val t_of_sexp : 'a 'b. (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) t

        [@@@ocaml.text "/*"]

        val sexp_of_t__stack
          : 'a 'b.
          ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('a, 'b) t -> Sexp.t

        [@@@ocaml.text "/*"]

        val sexp_of_t : 'a 'b. ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('a, 'b) t -> Sexp.t
      end

      include struct
        module type S3__any__value__value__stack = sig
          type ('a, 'b, 'c) t

          val t_of_sexp
            : 'a 'b 'c.
            (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c) -> Sexp.t -> ('a, 'b, 'c) t

          [@@@ocaml.text "/*"]

          val sexp_of_t__stack
            : 'a 'b 'c.
            ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('c -> Sexp.t) -> ('a, 'b, 'c) t -> Sexp.t

          [@@@ocaml.text "/*"]

          val sexp_of_t
            : 'a 'b 'c.
            ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('c -> Sexp.t) -> ('a, 'b, 'c) t -> Sexp.t
        end
      end [@@ocaml.doc " @inline "]

      include struct
        module type S3__any__value__any__stack = sig
          type ('a, 'b, 'c) t

          val t_of_sexp
            : 'a 'b 'c.
            (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c) -> Sexp.t -> ('a, 'b, 'c) t

          [@@@ocaml.text "/*"]

          val sexp_of_t__stack
            : 'a 'b 'c.
            ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('c -> Sexp.t) -> ('a, 'b, 'c) t -> Sexp.t

          [@@@ocaml.text "/*"]

          val sexp_of_t
            : 'a 'b 'c.
            ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('c -> Sexp.t) -> ('a, 'b, 'c) t -> Sexp.t
        end
      end [@@ocaml.doc " @inline "]
    end [@@ocaml.doc " @inline "]

    include struct
      module type S2__any__any__stack = sig
        type ('a, 'b) t

        val t_of_sexp : 'a 'b. (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) t

        [@@@ocaml.text "/*"]

        val sexp_of_t__stack
          : 'a 'b.
          ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('a, 'b) t -> Sexp.t

        [@@@ocaml.text "/*"]

        val sexp_of_t : 'a 'b. ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('a, 'b) t -> Sexp.t
      end

      include struct
        module type S3__any__any__value__stack = sig
          type ('a, 'b, 'c) t

          val t_of_sexp
            : 'a 'b 'c.
            (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c) -> Sexp.t -> ('a, 'b, 'c) t

          [@@@ocaml.text "/*"]

          val sexp_of_t__stack
            : 'a 'b 'c.
            ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('c -> Sexp.t) -> ('a, 'b, 'c) t -> Sexp.t

          [@@@ocaml.text "/*"]

          val sexp_of_t
            : 'a 'b 'c.
            ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('c -> Sexp.t) -> ('a, 'b, 'c) t -> Sexp.t
        end
      end [@@ocaml.doc " @inline "]

      include struct
        module type S3__any__any__any__stack = sig
          type ('a, 'b, 'c) t

          val t_of_sexp
            : 'a 'b 'c.
            (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c) -> Sexp.t -> ('a, 'b, 'c) t

          [@@@ocaml.text "/*"]

          val sexp_of_t__stack
            : 'a 'b 'c.
            ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('c -> Sexp.t) -> ('a, 'b, 'c) t -> Sexp.t

          [@@@ocaml.text "/*"]

          val sexp_of_t
            : 'a 'b 'c.
            ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('c -> Sexp.t) -> ('a, 'b, 'c) t -> Sexp.t
        end
      end [@@ocaml.doc " @inline "]
    end [@@ocaml.doc " @inline "]
  end [@@ocaml.doc " @inline "]
end [@@ocaml.doc " @inline "]

[@@@end]

module type S_with_grammar = sig
  include S

  val t_sexp_grammar : t Sexp_grammar.t
end

module type S1_with_grammar = sig
  include S1

  val t_sexp_grammar : 'a Sexp_grammar.t -> 'a t Sexp_grammar.t
end

module type S2_with_grammar = sig
  include S2

  val t_sexp_grammar : 'a Sexp_grammar.t -> 'b Sexp_grammar.t -> ('a, 'b) t Sexp_grammar.t
end

module type S3_with_grammar = sig
  include S3

  val t_sexp_grammar
    :  'a Sexp_grammar.t
    -> 'b Sexp_grammar.t
    -> 'c Sexp_grammar.t
    -> ('a, 'b, 'c) t Sexp_grammar.t
end
