open Basement

module Definitions = struct
  type t = Sexp_type.Sexp.t =
    | Atom of string
    | List of t list

  module type Pretty_print_to_formatter = sig @@ portable
    (** [pp_hum ppf sexp] outputs S-expression [sexp] to formatter [ppf] in human readable
        form. *)
    val pp_hum : Format.formatter -> t -> unit

    (** [pp_hum_indent n ppf sexp] outputs S-expression [sexp] to formatter [ppf] in human
        readable form and indentation level [n]. *)
    val pp_hum_indent : int -> Format.formatter -> t -> unit

    (** [pp_mach ppf sexp] outputs S-expression [sexp] to formatter [ppf] in machine
        readable (i.e. most compact) form. *)
    val pp_mach : Format.formatter -> t -> unit

    (** Same as [pp_mach]. *)
    val pp : Format.formatter -> t -> unit
  end

  module type Pretty_printing_helpers_private = sig @@ portable
    (** Functions used by [Make_pretty_printing] *)

    val mach_maybe_esc_str : string -> string
    val must_escape : string -> bool
    val esc_str : string -> string
  end

  module type Pretty_printing_helpers = sig @@ portable
    include Pretty_print_to_formatter (** @inline *)

    include Pretty_printing_helpers_private (** @inline *)
  end

  module type Pretty_printing = sig @@ portable
    (*_ In [Base], this is replaced with [String.Utf8.t] *)
    type output

    (** {1 Printing to formatters} *)

    include Pretty_print_to_formatter (** @inline *)

    (** {1 Conversion to strings} *)

    (** [to_string_hum ?indent ?max_width sexp] converts S-expression [sexp] to a string
        in human readable form with indentation level [indent] and target maximum width
        [max_width]. Note long atoms may overflow [max_width].

        @param indent default = [Dynamic.get default_indent]
        @param max_width default = [78] *)
    val to_string_hum : ?indent:int -> ?max_width:int -> t -> output

    (** [to_string_mach sexp] converts S-expression [sexp] to a string in machine readable
        (i.e. most compact) form. *)
    val to_string_mach : t -> output

    (** Same as [to_string_mach]. *)
    val to_string : t -> output

    (** {1 Conversion to buffers} *)

    (** [to_buffer_hum ~buf ?indent ?max_width sexp] outputs the S-expression [sexp]
        converted to a string in human readable form to buffer [buf] with indentation
        level [indent] and target maximum width [max_width]. Note long atoms may overflow
        [max_width].

        @param indent default = [Dynamic.get default_indent]
        @param max_width default = [78] *)
    val to_buffer_hum : buf:Buffer.t -> ?indent:int -> ?max_width:int -> t -> unit

    (** [to_buffer_mach ~buf sexp] outputs the S-expression [sexp] converted to a string
        in machine readable (i.e. most compact) form to buffer [buf]. *)
    val to_buffer_mach : buf:Buffer.t -> t -> unit

    (** [to_buffer ~buf sexp] same as {!to_buffer_mach}. *)
    val to_buffer : buf:Buffer.t -> t -> unit

    (** [to_buffer_gen ~buf ~add_char ~add_string sexp] outputs the S-expression [sexp]
        converted to a string to buffer [buf] using the output functions [add_char] and
        [add_string]. *)
    val to_buffer_gen
      :  buf:'buffer
      -> add_char:('buffer -> char -> unit)
      -> add_string:('buffer -> string -> unit)
      -> t
      -> unit

    (*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

        https://opensource.janestreet.com/standards/#private-submodules *)
    module Pretty_printing_helpers_private : Pretty_printing_helpers_private
  end
end

module type Sexp = sig @@ portable
  (*_ NOTE: We do not use the [include module type of struct] pattern here as it messes
      with the compiler's short-names heuristics. This should be okay since [Definitions]
      isn't exported from this library. *)
  include module type of Definitions

  (*_ We don't use [@@deriving sexp] as this would generated references to [Sexplib],
      creating a circular dependency *)
  val t_of_sexp : t -> t
  val sexp_of_t : t -> t
  val sexp_of_t__stack : local_ t -> local_ t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val globalize : t @ local -> t @ global

  (** [Not_found_s] is used by functions that historically raised [Not_found], to allow
      them to raise an exception that contains an informative error message (as a sexp),
      while still having an exception that can be distinguished from other exceptions. *)
  exception Not_found_s of t

  (** [Of_sexp_error (exn, sexp)] the exception raised when an S-expression could not be
      successfully converted to an OCaml-value. *)
  exception Of_sexp_error of exn * t

  (** {1 Helpers} *)

  (** {v
    Helper to build nice s-expressions for error messages.  It imitates the behavior of
    [[%message ...]] from the ppx_sexp_message rewriter.

    [message name key_values] produces a s-expression list starting with atom [name] and
    followed by list of size 2 of the form [(key value)].  When the key is the empty
    string, [value] is used directly instead as for [[%message]].

    For instance the following code:

    {[
      Sexp.message "error"
        [ "x", sexp_of_int 42
        ; "" , sexp_of_exn Exit
        ]
    ]}

    produces the s-expression:

    {[
      (error (x 42) Exit)
    ]}
      v} *)
  val message : string -> (string * t) list -> t

  (** {1 Defaults} *)

  (** [default_indent] reference to default indentation level for human-readable
      conversions.

      Initialisation value: 1. *)
  val default_indent : int Dynamic.t

  (** {1 Pretty printing of S-expressions} *)

  module Make_pretty_printing (Helpers : Pretty_printing_helpers) :
    Pretty_printing with type output := string

  include Pretty_printing with type output := string

  (** See [Pretty_printing.to_string_hum], [to_string_mach], and [to_string],
      respectively. *)

  (** WARNING [to_string_hum__stack] globalizes [t] if it is allocated on the stack. *)
  val to_string_hum__stack : ?indent:int -> ?max_width:int -> t @ local -> string @ local

  val to_string_mach__stack : t @ local -> string @ local
  val to_string__stack : t @ local -> string @ local

  (** {1 Styles} *)

  val of_float_style : [ `Underscores | `No_underscores ] Dynamic.t
  val of_int_style : [ `Underscores | `No_underscores ] Dynamic.t

  (*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

      https://opensource.janestreet.com/standards/#private-submodules *)
  module Private : sig
    (*_ Exported for sexplib *)

    val size : t -> int * int
    val buffer : unit -> Buffer.t

    include Definitions.Pretty_printing_helpers_private
    include Pretty_printing with type output := string
  end
end
