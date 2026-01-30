(lang dune 3.0)
(name plankalkul)
(version 0.1.0)

(generate_opam_files true)

(source (github Zaneham/plankalkul-compiler))
(license MIT)
(authors "Zane Hambly")
(maintainers "Zane Hambly")

(package
 (name plankalkul)
 (synopsis "A compiler for Konrad Zuse's Plankalkul (1942-1945)")
 (description "
The first high-level programming language, finally compiled to native code.
A French programming language (OCaml) giving new life to a German one.
Supports both original 2D notation and linearised syntax.
Features the full language: predicates, set operations, function templates,
and the complete type system Zuse envisioned.
Targets C for maximum portability.")
 (depends
  (ocaml (>= 4.14.0))
  (dune (>= 3.0))
  (ppx_deriving (>= 5.2))
  (cmdliner (>= 1.1.0))
  (alcotest :with-test)))
