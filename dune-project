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

(package
 (name cairos_finance)
 (synopsis "Financial metrics for Cairos time-series")
 (description "Cross-validated financial metrics built on Cairos core")
 (depends
  (ptime
   (and
    (>= 1.2.0)
    :with-test))
  (nx
   (and
    (>= 1.0.0~alpha2)
    :with-test))
  (fmt
   (and
    (>= 0.11.0)
    :with-test))
  cairos
  ocaml
  (alcotest :with-test)))
