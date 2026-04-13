val display : string -> unit
(** [display svg] renders the SVG string inline in the current Jupyter notebook
    cell by calling [Jupyter_notebook.display] with MIME type ["image/svg+xml"].

    This is the Jupyter adapter for {!Cairos_plot}. The typical usage in a
    notebook is:
    {[
      #require "cairos_jupyter"
      series
      |> Cairos_plot.line_chart ~title:"Price"
      |> Cairos_jupyter.display
    ]}

    This function is only usable inside a Jupyter kernel, which is enforced by
    the [cairos_jupyter] library being bytecode-only (it links directly against
    [jupyter.notebook]). *)
