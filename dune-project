(lang dune 2.2)
(name ocaml-elo)
(generate_opam_files true)

(package
 (name ocaml-elo)
 (synopsis "Ocaml elo")
 (description "")
 (depends
  ppx_deriving
  (containers (>= 3.0))
  (containers-data (>= 3.0))
  (dune (>= 2.0))
  (zarith (>= 1.9.1))
  (imandra-base (>= 1.1.0))
  (ocamlgraph (>= 2.0.0))))
