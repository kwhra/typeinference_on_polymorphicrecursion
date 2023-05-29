(lang dune 3.4)
(using menhir 2.1)
(name tyinf_polyrec)

(generate_opam_files true)

(source
 (github kwhra/typeinference_on_polymorphicrecursion.git))

(authors "Kazuma KAWAHARA")

(maintainers "Kazuma KAWAHARA")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name tyinf_polyrec)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune ocaml-lsp-server ocamlformat utop)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
