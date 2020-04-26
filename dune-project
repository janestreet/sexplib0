(lang dune 2.0)

(name sexplib0)

(version v0.14.0)

(generate_opam_files true)

(authors "Jane Street Group, LLC <opensource@janestreet.com>")

(maintainers "opensource@janestreet.com")

(license MIT)

(source
 (github janestreet/sexplib0))

(package
 (name sexplib0)
 (synopsis
   "Library containing the definition of S-expressions and some base converters")
 (description
   "Part of Jane Street's Core library\nThe Core suite of libraries is an industrial strength alternative to\nOCaml's standard library that was developed by Jane Street, the\nlargest industrial user of OCaml.")
 (depends
  (ocaml
   (>= 4.04.2))))
