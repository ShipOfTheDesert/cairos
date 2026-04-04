(lang dune 3.20)

(name cairos)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name cairos)
 (synopsis "Typed time-series library for OCaml")
 (description
  "Frequency-aware, indexed time-series with financial operations for quantitative analysis")
 (depends
  (fmt
   (and
    (>= 0.11.0)
    :with-test))
  ocaml
  ptime
  nx
  (alcotest :with-test)))