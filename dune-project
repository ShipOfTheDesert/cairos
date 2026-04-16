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
  (alcotest :with-test)
  (qcheck-core :with-test)
  (qcheck-alcotest :with-test)
  (bechamel :with-test)
  (bechamel-notty :with-test)))

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

(package
 (name cairos_plot)
 (synopsis "SVG chart rendering for Cairos time-series")
 (description
  "Line and drawdown charts via Nopal scene trees, emitted as pure SVG strings")
 (depends
  cairos
  nopal_svg
  nopal_scene
  nopal_draw
  nopal_style
  nopal_charts
  ocaml
  (fmt
   (and
    (>= 0.11.0)
    :with-test))
  (nx
   (and
    (>= 1.0.0~alpha2)
    :with-test))
  (ptime
   (>= 1.2.0))
  (alcotest :with-test)))

(package
 (name cairos_jupyter)
 (synopsis "Jupyter notebook adapter for Cairos")
 (description
  "Bytecode-only adapter that renders Cairos_plot SVG output inline in Jupyter cells and provides pretty-printers for Cairos.Series and Cairos.Frame values")
 (depends cairos cairos_plot nx jupyter ocaml))