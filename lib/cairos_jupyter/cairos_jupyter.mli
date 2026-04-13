(** Jupyter notebook adapter for Cairos.

    This library is the Jupyter integration layer for the Cairos ecosystem. It
    is bytecode-only and is only meant to be loaded inside an [ocaml-jupyter]
    kernel. It provides two categories of functionality:

    - {!display}: renders an SVG string from {!Cairos_plot} inline in the
      current notebook cell.
    - The [pp_*] family: prints summaries of {!Cairos.Series.t},
      {!Cairos.Frame.t}, and descriptive statistics directly to the cell output
      via [Printf].

    {1 Load-time side effect: stdout buffering}

    Loading this module runs a single top-level effect:
    {[
      let () = Out_channel.set_buffered stdout false
    ]}
    The [ocaml-jupyter] kernel captures [stdout] through a pipe, which makes
    OCaml default to full buffering — without this, [Printf] output never
    flushes and never appears in the cell until the kernel shuts down. Disabling
    buffering means every print reaches the kernel immediately without requiring
    [flush stdout] or [%!] format directives in user code or in the [pp_*]
    helpers below.

    This is intentional. [Cairos_jupyter] is Jupyter-only by construction — the
    library is bytecode-only and links [jupyter.notebook] directly — so the
    effect only runs inside a Jupyter kernel. No non-Jupyter consumer can link
    this library.

    {2 Restoring buffered stdout}

    If a specific notebook needs buffered [stdout] (e.g. to benchmark throughput
    of a tight loop that prints), re-enable it explicitly in that notebook after
    loading [cairos_jupyter]:
    {[
      let () = Out_channel.set_buffered stdout true
    ]}
    Remember that with buffered [stdout], [Printf] output will not appear in the
    cell unless you manually [flush stdout] or end your format strings with
    [%!]. *)

val display : string -> unit
(** [display svg] renders the SVG string inline in the current Jupyter notebook
    cell by calling [Jupyter_notebook.display] with MIME type ["image/svg+xml"].

    Typical usage:
    {[
      series |> Cairos_plot.line_chart ~title:"Price" |> Cairos_jupyter.display
    ]} *)

val pp_series :
  ?n:int -> string -> ('freq, (float, 'b) Nx.t) Cairos.Series.t -> unit
(** [pp_series ?n name s] prints a summary of series [s] to stdout: its length,
    the first [n] values, and the last [n] values. Defaults to [n = 3]. *)

val pp_first_valid : string -> ('freq, (float, 'b) Nx.t) Cairos.Series.t -> unit
(** [pp_first_valid name s] prints the index and value of the first non-NaN
    element in [s], or ["all NaN"] if none exists. *)

val pp_frame : ?n:int -> 'freq Cairos.Frame.t -> unit
(** [pp_frame ?n frame] prints the column names of [frame] and the last [n]
    values of each column. Defaults to [n = 3]. *)

val pp_describe : 'freq Cairos.Frame.t -> unit
(** [pp_describe frame] prints a table of descriptive statistics (count, mean,
    std, min, p25, median, p75, max) for every column in [frame]. *)
