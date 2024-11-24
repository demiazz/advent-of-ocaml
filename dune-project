(lang dune 3.16)

(using melange 0.1)

(name advent-of-ocaml)

(generate_opam_files true)

(source
 (github demiazz/advent-of-ocaml))

(authors "Alexey Plutalov")

(maintainers "demiazz.py@gmail.com")

(license ISC)

(documentation https://github.com/demiazz/advent-of-ocaml#readme)

(package
 (name advent-of-ocaml)
 (synopsis "Advent of Code on OCaml")
 (description "Solutions of tasks from Advent of Code events.")
 (depends
  (ocaml
   (>= 5.2.0))
  (reason
   (>= 3.13.0))
  (melange
   (>= 4.0.1))
  (dune
   (>= 3.16.1))
  (reason-react
   (>= 0.15.0))
  (reason-react-ppx
   (>= 0.15.0))
  (opam-check-npm-deps
   (and
    :build
    (>= 3.0.1)))
  (ocaml-lsp-server :dev)
  (ocamlformat :dev)
  (dot-merlin-reader :dev)
  (utop :dev)
  (alcotest
   (and
    :with-test
    (>= 1.8.0)))
  (odoc :with-doc))
 (tags ("advent-of-code")))
