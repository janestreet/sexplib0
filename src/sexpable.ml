[@@@expand_inline
  [%%template
  module type Of_sexp = sig
    type t : any

    val t_of_sexp : Sexp.t -> t
  end

  [@@@alloc.default a @ m = (heap @ global, stack @ local)]

  module type Sexp_of = sig
    type t : any

    val sexp_of_t : t @ m -> Sexp.t @ m [@@alloc a @ m = (a @ m, heap @ global)]
  end

  module type S_any = sig
    type t : any

    include Of_sexp with type t := t
    include Sexp_of [@alloc a] with type t := t
  end

  module type S = sig
    type t

    include S_any [@alloc a] with type t := t
  end

  [@@@kind.default ka = (value, any)]

  module type S_any1 = sig
    type ('a : ka) t : any

    val t_of_sexp : ('a : ka). (Sexp.t -> 'a) -> Sexp.t -> 'a t

    val sexp_of_t : ('a : ka). ('a @ m -> Sexp.t @ m) -> 'a t @ m -> Sexp.t @ m
    [@@alloc a @ m = (a @ m, heap @ global)]
  end

  module type S1 = sig
    type ('a : ka) t

    include S_any1 [@kind ka] [@alloc a] with type ('a : ka) t := 'a t
  end

  [@@@kind.default kb = (value, any)]

  module type S_any2 = sig
    type ('a : ka, 'b : kb) t : any

    val t_of_sexp
      : ('a : ka) ('b : kb).
      (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) t

    val sexp_of_t
      : ('a : ka) ('b : kb).
      ('a @ m -> Sexp.t @ m) -> ('b @ m -> Sexp.t @ m) -> ('a, 'b) t @ m -> Sexp.t @ m
    [@@alloc a @ m = (a @ m, heap @ global)]
  end

  module type S2 = sig
    type ('a : ka, 'b : kb) t

    include S_any2 [@kind ka kb] [@alloc a] with type ('a : ka, 'b : kb) t := ('a, 'b) t
  end

  [@@@kind.default kc = (value, any)]

  module type S_any3 = sig
    type ('a : ka, 'b : kb, 'c : kc) t : any

    val t_of_sexp
      : ('a : ka) ('b : kb) ('c : kc).
      (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c) -> Sexp.t -> ('a, 'b, 'c) t

    val sexp_of_t
      : ('a : ka) ('b : kb) ('c : kc).
      ('a @ m -> Sexp.t @ m)
      -> ('b @ m -> Sexp.t @ m)
      -> ('c @ m -> Sexp.t @ m)
      -> ('a, 'b, 'c) t @ m
      -> Sexp.t @ m
    [@@alloc a @ m = (a @ m, heap @ global)]
  end

  module type S3 = sig
    type ('a : ka, 'b : kb, 'c : kc) t

    include
      S_any3
      [@kind ka kb kc] [@alloc a]
      with type ('a : ka, 'b : kb, 'c : kc) t := ('a, 'b, 'c) t
  end]]

module type Of_sexp = sig
  type t : any

  val t_of_sexp : Sexp.t -> t
end

include struct
  module type Sexp_of = sig
    type t : any

    val sexp_of_t : t @ global -> Sexp.t @ global
  end

  module type S_any = sig
    type t : any

    include Of_sexp with type t := t
    include Sexp_of with type t := t
  end

  module type S = sig
    type t

    include S_any with type t := t
  end

  include struct
    module type S_any1 = sig
      type ('a : value) t : any

      val t_of_sexp : ('a : value). (Sexp.t -> 'a) -> Sexp.t -> 'a t

      val sexp_of_t
        : ('a : value).
        ('a @ global -> Sexp.t @ global) -> 'a t @ global -> Sexp.t @ global
    end

    module type S1 = sig
      type ('a : value) t

      include S_any1 with type ('a : value) t := 'a t
    end

    include struct
      module type S_any2 = sig
        type ('a : value, 'b : value) t : any

        val t_of_sexp
          : ('a : value) ('b : value).
          (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) t

        val sexp_of_t
          : ('a : value) ('b : value).
          ('a @ global -> Sexp.t @ global)
          -> ('b @ global -> Sexp.t @ global)
          -> ('a, 'b) t @ global
          -> Sexp.t @ global
      end

      module type S2 = sig
        type ('a : value, 'b : value) t

        include S_any2 with type ('a : value, 'b : value) t := ('a, 'b) t
      end

      include struct
        module type S_any3 = sig
          type ('a : value, 'b : value, 'c : value) t : any

          val t_of_sexp
            : ('a : value) ('b : value) ('c : value).
            (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c) -> Sexp.t -> ('a, 'b, 'c) t

          val sexp_of_t
            : ('a : value) ('b : value) ('c : value).
            ('a @ global -> Sexp.t @ global)
            -> ('b @ global -> Sexp.t @ global)
            -> ('c @ global -> Sexp.t @ global)
            -> ('a, 'b, 'c) t @ global
            -> Sexp.t @ global
        end

        module type S3 = sig
          type ('a : value, 'b : value, 'c : value) t

          include
            S_any3 with type ('a : value, 'b : value, 'c : value) t := ('a, 'b, 'c) t
        end
      end [@@ocaml.doc " @inline "]

      include struct
        module type S_any3__value__value__any = sig
          type ('a : value, 'b : value, 'c : any) t : any

          val t_of_sexp
            : ('a : value) ('b : value) ('c : any).
            (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c) -> Sexp.t -> ('a, 'b, 'c) t

          val sexp_of_t
            : ('a : value) ('b : value) ('c : any).
            ('a @ global -> Sexp.t @ global)
            -> ('b @ global -> Sexp.t @ global)
            -> ('c @ global -> Sexp.t @ global)
            -> ('a, 'b, 'c) t @ global
            -> Sexp.t @ global
        end

        module type S3__value__value__any = sig
          type ('a : value, 'b : value, 'c : any) t

          include
            S_any3__value__value__any
            with type ('a : value, 'b : value, 'c : any) t := ('a, 'b, 'c) t
        end
      end [@@ocaml.doc " @inline "]
    end [@@ocaml.doc " @inline "]

    include struct
      module type S_any2__value__any = sig
        type ('a : value, 'b : any) t : any

        val t_of_sexp
          : ('a : value) ('b : any).
          (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) t

        val sexp_of_t
          : ('a : value) ('b : any).
          ('a @ global -> Sexp.t @ global)
          -> ('b @ global -> Sexp.t @ global)
          -> ('a, 'b) t @ global
          -> Sexp.t @ global
      end

      module type S2__value__any = sig
        type ('a : value, 'b : any) t

        include S_any2__value__any with type ('a : value, 'b : any) t := ('a, 'b) t
      end

      include struct
        module type S_any3__value__any__value = sig
          type ('a : value, 'b : any, 'c : value) t : any

          val t_of_sexp
            : ('a : value) ('b : any) ('c : value).
            (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c) -> Sexp.t -> ('a, 'b, 'c) t

          val sexp_of_t
            : ('a : value) ('b : any) ('c : value).
            ('a @ global -> Sexp.t @ global)
            -> ('b @ global -> Sexp.t @ global)
            -> ('c @ global -> Sexp.t @ global)
            -> ('a, 'b, 'c) t @ global
            -> Sexp.t @ global
        end

        module type S3__value__any__value = sig
          type ('a : value, 'b : any, 'c : value) t

          include
            S_any3__value__any__value
            with type ('a : value, 'b : any, 'c : value) t := ('a, 'b, 'c) t
        end
      end [@@ocaml.doc " @inline "]

      include struct
        module type S_any3__value__any__any = sig
          type ('a : value, 'b : any, 'c : any) t : any

          val t_of_sexp
            : ('a : value) ('b : any) ('c : any).
            (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c) -> Sexp.t -> ('a, 'b, 'c) t

          val sexp_of_t
            : ('a : value) ('b : any) ('c : any).
            ('a @ global -> Sexp.t @ global)
            -> ('b @ global -> Sexp.t @ global)
            -> ('c @ global -> Sexp.t @ global)
            -> ('a, 'b, 'c) t @ global
            -> Sexp.t @ global
        end

        module type S3__value__any__any = sig
          type ('a : value, 'b : any, 'c : any) t

          include
            S_any3__value__any__any
            with type ('a : value, 'b : any, 'c : any) t := ('a, 'b, 'c) t
        end
      end [@@ocaml.doc " @inline "]
    end [@@ocaml.doc " @inline "]
  end [@@ocaml.doc " @inline "]

  include struct
    module type S_any1__any = sig
      type ('a : any) t : any

      val t_of_sexp : ('a : any). (Sexp.t -> 'a) -> Sexp.t -> 'a t

      val sexp_of_t
        : ('a : any).
        ('a @ global -> Sexp.t @ global) -> 'a t @ global -> Sexp.t @ global
    end

    module type S1__any = sig
      type ('a : any) t

      include S_any1__any with type ('a : any) t := 'a t
    end

    include struct
      module type S_any2__any__value = sig
        type ('a : any, 'b : value) t : any

        val t_of_sexp
          : ('a : any) ('b : value).
          (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) t

        val sexp_of_t
          : ('a : any) ('b : value).
          ('a @ global -> Sexp.t @ global)
          -> ('b @ global -> Sexp.t @ global)
          -> ('a, 'b) t @ global
          -> Sexp.t @ global
      end

      module type S2__any__value = sig
        type ('a : any, 'b : value) t

        include S_any2__any__value with type ('a : any, 'b : value) t := ('a, 'b) t
      end

      include struct
        module type S_any3__any__value__value = sig
          type ('a : any, 'b : value, 'c : value) t : any

          val t_of_sexp
            : ('a : any) ('b : value) ('c : value).
            (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c) -> Sexp.t -> ('a, 'b, 'c) t

          val sexp_of_t
            : ('a : any) ('b : value) ('c : value).
            ('a @ global -> Sexp.t @ global)
            -> ('b @ global -> Sexp.t @ global)
            -> ('c @ global -> Sexp.t @ global)
            -> ('a, 'b, 'c) t @ global
            -> Sexp.t @ global
        end

        module type S3__any__value__value = sig
          type ('a : any, 'b : value, 'c : value) t

          include
            S_any3__any__value__value
            with type ('a : any, 'b : value, 'c : value) t := ('a, 'b, 'c) t
        end
      end [@@ocaml.doc " @inline "]

      include struct
        module type S_any3__any__value__any = sig
          type ('a : any, 'b : value, 'c : any) t : any

          val t_of_sexp
            : ('a : any) ('b : value) ('c : any).
            (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c) -> Sexp.t -> ('a, 'b, 'c) t

          val sexp_of_t
            : ('a : any) ('b : value) ('c : any).
            ('a @ global -> Sexp.t @ global)
            -> ('b @ global -> Sexp.t @ global)
            -> ('c @ global -> Sexp.t @ global)
            -> ('a, 'b, 'c) t @ global
            -> Sexp.t @ global
        end

        module type S3__any__value__any = sig
          type ('a : any, 'b : value, 'c : any) t

          include
            S_any3__any__value__any
            with type ('a : any, 'b : value, 'c : any) t := ('a, 'b, 'c) t
        end
      end [@@ocaml.doc " @inline "]
    end [@@ocaml.doc " @inline "]

    include struct
      module type S_any2__any__any = sig
        type ('a : any, 'b : any) t : any

        val t_of_sexp
          : ('a : any) ('b : any).
          (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) t

        val sexp_of_t
          : ('a : any) ('b : any).
          ('a @ global -> Sexp.t @ global)
          -> ('b @ global -> Sexp.t @ global)
          -> ('a, 'b) t @ global
          -> Sexp.t @ global
      end

      module type S2__any__any = sig
        type ('a : any, 'b : any) t

        include S_any2__any__any with type ('a : any, 'b : any) t := ('a, 'b) t
      end

      include struct
        module type S_any3__any__any__value = sig
          type ('a : any, 'b : any, 'c : value) t : any

          val t_of_sexp
            : ('a : any) ('b : any) ('c : value).
            (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c) -> Sexp.t -> ('a, 'b, 'c) t

          val sexp_of_t
            : ('a : any) ('b : any) ('c : value).
            ('a @ global -> Sexp.t @ global)
            -> ('b @ global -> Sexp.t @ global)
            -> ('c @ global -> Sexp.t @ global)
            -> ('a, 'b, 'c) t @ global
            -> Sexp.t @ global
        end

        module type S3__any__any__value = sig
          type ('a : any, 'b : any, 'c : value) t

          include
            S_any3__any__any__value
            with type ('a : any, 'b : any, 'c : value) t := ('a, 'b, 'c) t
        end
      end [@@ocaml.doc " @inline "]

      include struct
        module type S_any3__any__any__any = sig
          type ('a : any, 'b : any, 'c : any) t : any

          val t_of_sexp
            : ('a : any) ('b : any) ('c : any).
            (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c) -> Sexp.t -> ('a, 'b, 'c) t

          val sexp_of_t
            : ('a : any) ('b : any) ('c : any).
            ('a @ global -> Sexp.t @ global)
            -> ('b @ global -> Sexp.t @ global)
            -> ('c @ global -> Sexp.t @ global)
            -> ('a, 'b, 'c) t @ global
            -> Sexp.t @ global
        end

        module type S3__any__any__any = sig
          type ('a : any, 'b : any, 'c : any) t

          include
            S_any3__any__any__any
            with type ('a : any, 'b : any, 'c : any) t := ('a, 'b, 'c) t
        end
      end [@@ocaml.doc " @inline "]
    end [@@ocaml.doc " @inline "]
  end [@@ocaml.doc " @inline "]
end [@@ocaml.doc " @inline "]

include struct
  module type Sexp_of__stack = sig
    type t : any

    [@@@ocaml.text "/*"]

    val sexp_of_t__stack : local_ t -> local_ Sexp.t

    [@@@ocaml.text "/*"]

    val sexp_of_t : t @ global -> Sexp.t @ global
  end

  module type S_any__stack = sig
    type t : any

    include Of_sexp with type t := t
    include Sexp_of__stack with type t := t
  end

  module type S__stack = sig
    type t

    include S_any__stack with type t := t
  end

  include struct
    module type S_any1__stack = sig
      type ('a : value) t : any

      val t_of_sexp : ('a : value). (Sexp.t -> 'a) -> Sexp.t -> 'a t

      [@@@ocaml.text "/*"]

      val sexp_of_t__stack
        : ('a : value).
        (local_ 'a -> local_ Sexp.t) -> local_ 'a t -> local_ Sexp.t

      [@@@ocaml.text "/*"]

      val sexp_of_t
        : ('a : value).
        ('a @ global -> Sexp.t @ global) -> 'a t @ global -> Sexp.t @ global
    end

    module type S1__stack = sig
      type ('a : value) t

      include S_any1__stack with type ('a : value) t := 'a t
    end

    include struct
      module type S_any2__stack = sig
        type ('a : value, 'b : value) t : any

        val t_of_sexp
          : ('a : value) ('b : value).
          (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) t

        [@@@ocaml.text "/*"]

        val sexp_of_t__stack
          : ('a : value) ('b : value).
          (local_ 'a -> local_ Sexp.t)
          -> (local_ 'b -> local_ Sexp.t)
          -> local_ ('a, 'b) t
          -> local_ Sexp.t

        [@@@ocaml.text "/*"]

        val sexp_of_t
          : ('a : value) ('b : value).
          ('a @ global -> Sexp.t @ global)
          -> ('b @ global -> Sexp.t @ global)
          -> ('a, 'b) t @ global
          -> Sexp.t @ global
      end

      module type S2__stack = sig
        type ('a : value, 'b : value) t

        include S_any2__stack with type ('a : value, 'b : value) t := ('a, 'b) t
      end

      include struct
        module type S_any3__stack = sig
          type ('a : value, 'b : value, 'c : value) t : any

          val t_of_sexp
            : ('a : value) ('b : value) ('c : value).
            (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c) -> Sexp.t -> ('a, 'b, 'c) t

          [@@@ocaml.text "/*"]

          val sexp_of_t__stack
            : ('a : value) ('b : value) ('c : value).
            (local_ 'a -> local_ Sexp.t)
            -> (local_ 'b -> local_ Sexp.t)
            -> (local_ 'c -> local_ Sexp.t)
            -> local_ ('a, 'b, 'c) t
            -> local_ Sexp.t

          [@@@ocaml.text "/*"]

          val sexp_of_t
            : ('a : value) ('b : value) ('c : value).
            ('a @ global -> Sexp.t @ global)
            -> ('b @ global -> Sexp.t @ global)
            -> ('c @ global -> Sexp.t @ global)
            -> ('a, 'b, 'c) t @ global
            -> Sexp.t @ global
        end

        module type S3__stack = sig
          type ('a : value, 'b : value, 'c : value) t

          include
            S_any3__stack
            with type ('a : value, 'b : value, 'c : value) t := ('a, 'b, 'c) t
        end
      end [@@ocaml.doc " @inline "]

      include struct
        module type S_any3__value__value__any__stack = sig
          type ('a : value, 'b : value, 'c : any) t : any

          val t_of_sexp
            : ('a : value) ('b : value) ('c : any).
            (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c) -> Sexp.t -> ('a, 'b, 'c) t

          [@@@ocaml.text "/*"]

          val sexp_of_t__stack
            : ('a : value) ('b : value) ('c : any).
            (local_ 'a -> local_ Sexp.t)
            -> (local_ 'b -> local_ Sexp.t)
            -> (local_ 'c -> local_ Sexp.t)
            -> local_ ('a, 'b, 'c) t
            -> local_ Sexp.t

          [@@@ocaml.text "/*"]

          val sexp_of_t
            : ('a : value) ('b : value) ('c : any).
            ('a @ global -> Sexp.t @ global)
            -> ('b @ global -> Sexp.t @ global)
            -> ('c @ global -> Sexp.t @ global)
            -> ('a, 'b, 'c) t @ global
            -> Sexp.t @ global
        end

        module type S3__value__value__any__stack = sig
          type ('a : value, 'b : value, 'c : any) t

          include
            S_any3__value__value__any__stack
            with type ('a : value, 'b : value, 'c : any) t := ('a, 'b, 'c) t
        end
      end [@@ocaml.doc " @inline "]
    end [@@ocaml.doc " @inline "]

    include struct
      module type S_any2__value__any__stack = sig
        type ('a : value, 'b : any) t : any

        val t_of_sexp
          : ('a : value) ('b : any).
          (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) t

        [@@@ocaml.text "/*"]

        val sexp_of_t__stack
          : ('a : value) ('b : any).
          (local_ 'a -> local_ Sexp.t)
          -> (local_ 'b -> local_ Sexp.t)
          -> local_ ('a, 'b) t
          -> local_ Sexp.t

        [@@@ocaml.text "/*"]

        val sexp_of_t
          : ('a : value) ('b : any).
          ('a @ global -> Sexp.t @ global)
          -> ('b @ global -> Sexp.t @ global)
          -> ('a, 'b) t @ global
          -> Sexp.t @ global
      end

      module type S2__value__any__stack = sig
        type ('a : value, 'b : any) t

        include S_any2__value__any__stack with type ('a : value, 'b : any) t := ('a, 'b) t
      end

      include struct
        module type S_any3__value__any__value__stack = sig
          type ('a : value, 'b : any, 'c : value) t : any

          val t_of_sexp
            : ('a : value) ('b : any) ('c : value).
            (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c) -> Sexp.t -> ('a, 'b, 'c) t

          [@@@ocaml.text "/*"]

          val sexp_of_t__stack
            : ('a : value) ('b : any) ('c : value).
            (local_ 'a -> local_ Sexp.t)
            -> (local_ 'b -> local_ Sexp.t)
            -> (local_ 'c -> local_ Sexp.t)
            -> local_ ('a, 'b, 'c) t
            -> local_ Sexp.t

          [@@@ocaml.text "/*"]

          val sexp_of_t
            : ('a : value) ('b : any) ('c : value).
            ('a @ global -> Sexp.t @ global)
            -> ('b @ global -> Sexp.t @ global)
            -> ('c @ global -> Sexp.t @ global)
            -> ('a, 'b, 'c) t @ global
            -> Sexp.t @ global
        end

        module type S3__value__any__value__stack = sig
          type ('a : value, 'b : any, 'c : value) t

          include
            S_any3__value__any__value__stack
            with type ('a : value, 'b : any, 'c : value) t := ('a, 'b, 'c) t
        end
      end [@@ocaml.doc " @inline "]

      include struct
        module type S_any3__value__any__any__stack = sig
          type ('a : value, 'b : any, 'c : any) t : any

          val t_of_sexp
            : ('a : value) ('b : any) ('c : any).
            (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c) -> Sexp.t -> ('a, 'b, 'c) t

          [@@@ocaml.text "/*"]

          val sexp_of_t__stack
            : ('a : value) ('b : any) ('c : any).
            (local_ 'a -> local_ Sexp.t)
            -> (local_ 'b -> local_ Sexp.t)
            -> (local_ 'c -> local_ Sexp.t)
            -> local_ ('a, 'b, 'c) t
            -> local_ Sexp.t

          [@@@ocaml.text "/*"]

          val sexp_of_t
            : ('a : value) ('b : any) ('c : any).
            ('a @ global -> Sexp.t @ global)
            -> ('b @ global -> Sexp.t @ global)
            -> ('c @ global -> Sexp.t @ global)
            -> ('a, 'b, 'c) t @ global
            -> Sexp.t @ global
        end

        module type S3__value__any__any__stack = sig
          type ('a : value, 'b : any, 'c : any) t

          include
            S_any3__value__any__any__stack
            with type ('a : value, 'b : any, 'c : any) t := ('a, 'b, 'c) t
        end
      end [@@ocaml.doc " @inline "]
    end [@@ocaml.doc " @inline "]
  end [@@ocaml.doc " @inline "]

  include struct
    module type S_any1__any__stack = sig
      type ('a : any) t : any

      val t_of_sexp : ('a : any). (Sexp.t -> 'a) -> Sexp.t -> 'a t

      [@@@ocaml.text "/*"]

      val sexp_of_t__stack
        : ('a : any).
        (local_ 'a -> local_ Sexp.t) -> local_ 'a t -> local_ Sexp.t

      [@@@ocaml.text "/*"]

      val sexp_of_t
        : ('a : any).
        ('a @ global -> Sexp.t @ global) -> 'a t @ global -> Sexp.t @ global
    end

    module type S1__any__stack = sig
      type ('a : any) t

      include S_any1__any__stack with type ('a : any) t := 'a t
    end

    include struct
      module type S_any2__any__value__stack = sig
        type ('a : any, 'b : value) t : any

        val t_of_sexp
          : ('a : any) ('b : value).
          (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) t

        [@@@ocaml.text "/*"]

        val sexp_of_t__stack
          : ('a : any) ('b : value).
          (local_ 'a -> local_ Sexp.t)
          -> (local_ 'b -> local_ Sexp.t)
          -> local_ ('a, 'b) t
          -> local_ Sexp.t

        [@@@ocaml.text "/*"]

        val sexp_of_t
          : ('a : any) ('b : value).
          ('a @ global -> Sexp.t @ global)
          -> ('b @ global -> Sexp.t @ global)
          -> ('a, 'b) t @ global
          -> Sexp.t @ global
      end

      module type S2__any__value__stack = sig
        type ('a : any, 'b : value) t

        include S_any2__any__value__stack with type ('a : any, 'b : value) t := ('a, 'b) t
      end

      include struct
        module type S_any3__any__value__value__stack = sig
          type ('a : any, 'b : value, 'c : value) t : any

          val t_of_sexp
            : ('a : any) ('b : value) ('c : value).
            (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c) -> Sexp.t -> ('a, 'b, 'c) t

          [@@@ocaml.text "/*"]

          val sexp_of_t__stack
            : ('a : any) ('b : value) ('c : value).
            (local_ 'a -> local_ Sexp.t)
            -> (local_ 'b -> local_ Sexp.t)
            -> (local_ 'c -> local_ Sexp.t)
            -> local_ ('a, 'b, 'c) t
            -> local_ Sexp.t

          [@@@ocaml.text "/*"]

          val sexp_of_t
            : ('a : any) ('b : value) ('c : value).
            ('a @ global -> Sexp.t @ global)
            -> ('b @ global -> Sexp.t @ global)
            -> ('c @ global -> Sexp.t @ global)
            -> ('a, 'b, 'c) t @ global
            -> Sexp.t @ global
        end

        module type S3__any__value__value__stack = sig
          type ('a : any, 'b : value, 'c : value) t

          include
            S_any3__any__value__value__stack
            with type ('a : any, 'b : value, 'c : value) t := ('a, 'b, 'c) t
        end
      end [@@ocaml.doc " @inline "]

      include struct
        module type S_any3__any__value__any__stack = sig
          type ('a : any, 'b : value, 'c : any) t : any

          val t_of_sexp
            : ('a : any) ('b : value) ('c : any).
            (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c) -> Sexp.t -> ('a, 'b, 'c) t

          [@@@ocaml.text "/*"]

          val sexp_of_t__stack
            : ('a : any) ('b : value) ('c : any).
            (local_ 'a -> local_ Sexp.t)
            -> (local_ 'b -> local_ Sexp.t)
            -> (local_ 'c -> local_ Sexp.t)
            -> local_ ('a, 'b, 'c) t
            -> local_ Sexp.t

          [@@@ocaml.text "/*"]

          val sexp_of_t
            : ('a : any) ('b : value) ('c : any).
            ('a @ global -> Sexp.t @ global)
            -> ('b @ global -> Sexp.t @ global)
            -> ('c @ global -> Sexp.t @ global)
            -> ('a, 'b, 'c) t @ global
            -> Sexp.t @ global
        end

        module type S3__any__value__any__stack = sig
          type ('a : any, 'b : value, 'c : any) t

          include
            S_any3__any__value__any__stack
            with type ('a : any, 'b : value, 'c : any) t := ('a, 'b, 'c) t
        end
      end [@@ocaml.doc " @inline "]
    end [@@ocaml.doc " @inline "]

    include struct
      module type S_any2__any__any__stack = sig
        type ('a : any, 'b : any) t : any

        val t_of_sexp
          : ('a : any) ('b : any).
          (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) t

        [@@@ocaml.text "/*"]

        val sexp_of_t__stack
          : ('a : any) ('b : any).
          (local_ 'a -> local_ Sexp.t)
          -> (local_ 'b -> local_ Sexp.t)
          -> local_ ('a, 'b) t
          -> local_ Sexp.t

        [@@@ocaml.text "/*"]

        val sexp_of_t
          : ('a : any) ('b : any).
          ('a @ global -> Sexp.t @ global)
          -> ('b @ global -> Sexp.t @ global)
          -> ('a, 'b) t @ global
          -> Sexp.t @ global
      end

      module type S2__any__any__stack = sig
        type ('a : any, 'b : any) t

        include S_any2__any__any__stack with type ('a : any, 'b : any) t := ('a, 'b) t
      end

      include struct
        module type S_any3__any__any__value__stack = sig
          type ('a : any, 'b : any, 'c : value) t : any

          val t_of_sexp
            : ('a : any) ('b : any) ('c : value).
            (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c) -> Sexp.t -> ('a, 'b, 'c) t

          [@@@ocaml.text "/*"]

          val sexp_of_t__stack
            : ('a : any) ('b : any) ('c : value).
            (local_ 'a -> local_ Sexp.t)
            -> (local_ 'b -> local_ Sexp.t)
            -> (local_ 'c -> local_ Sexp.t)
            -> local_ ('a, 'b, 'c) t
            -> local_ Sexp.t

          [@@@ocaml.text "/*"]

          val sexp_of_t
            : ('a : any) ('b : any) ('c : value).
            ('a @ global -> Sexp.t @ global)
            -> ('b @ global -> Sexp.t @ global)
            -> ('c @ global -> Sexp.t @ global)
            -> ('a, 'b, 'c) t @ global
            -> Sexp.t @ global
        end

        module type S3__any__any__value__stack = sig
          type ('a : any, 'b : any, 'c : value) t

          include
            S_any3__any__any__value__stack
            with type ('a : any, 'b : any, 'c : value) t := ('a, 'b, 'c) t
        end
      end [@@ocaml.doc " @inline "]

      include struct
        module type S_any3__any__any__any__stack = sig
          type ('a : any, 'b : any, 'c : any) t : any

          val t_of_sexp
            : ('a : any) ('b : any) ('c : any).
            (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c) -> Sexp.t -> ('a, 'b, 'c) t

          [@@@ocaml.text "/*"]

          val sexp_of_t__stack
            : ('a : any) ('b : any) ('c : any).
            (local_ 'a -> local_ Sexp.t)
            -> (local_ 'b -> local_ Sexp.t)
            -> (local_ 'c -> local_ Sexp.t)
            -> local_ ('a, 'b, 'c) t
            -> local_ Sexp.t

          [@@@ocaml.text "/*"]

          val sexp_of_t
            : ('a : any) ('b : any) ('c : any).
            ('a @ global -> Sexp.t @ global)
            -> ('b @ global -> Sexp.t @ global)
            -> ('c @ global -> Sexp.t @ global)
            -> ('a, 'b, 'c) t @ global
            -> Sexp.t @ global
        end

        module type S3__any__any__any__stack = sig
          type ('a : any, 'b : any, 'c : any) t

          include
            S_any3__any__any__any__stack
            with type ('a : any, 'b : any, 'c : any) t := ('a, 'b, 'c) t
        end
      end [@@ocaml.doc " @inline "]
    end [@@ocaml.doc " @inline "]
  end [@@ocaml.doc " @inline "]
end [@@ocaml.doc " @inline "]

[@@@end]

module type S_with_grammar = sig
  include S

  val t_sexp_grammar : t Sexp_grammar.t @@ portable
end

module type S1_with_grammar = sig
  include S1

  val t_sexp_grammar : 'a Sexp_grammar.t -> 'a t Sexp_grammar.t @@ portable
end

module type S2_with_grammar = sig
  include S2

  val t_sexp_grammar
    :  'a Sexp_grammar.t
    -> 'b Sexp_grammar.t
    -> ('a, 'b) t Sexp_grammar.t
    @@ portable
end

module type S3_with_grammar = sig
  include S3

  val t_sexp_grammar
    :  'a Sexp_grammar.t
    -> 'b Sexp_grammar.t
    -> 'c Sexp_grammar.t
    -> ('a, 'b, 'c) t Sexp_grammar.t
    @@ portable
end
