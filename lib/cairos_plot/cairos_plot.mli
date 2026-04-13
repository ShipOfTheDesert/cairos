val line_chart :
  ?title:string ->
  ?width:float ->
  ?height:float ->
  ('freq, (float, 'b) Nx.t) Cairos.Series.t ->
  string
(** [line_chart ?title ?width ?height series] renders [series] as a line chart
    and returns a well-formed SVG 1.1 string.

    The chart draws the series values as a polyline with labelled X and Y axes.
    X-axis labels are derived from the series index timestamps.

    @param title Optional chart title displayed above the plot area
    @param width SVG width in pixels (default [800.0])
    @param height SVG height in pixels (default [400.0]) *)

val drawdown_chart :
  ?title:string ->
  ?width:float ->
  ?height:float ->
  ('freq, (float, 'b) Nx.t) Cairos.Series.t ->
  string
(** [drawdown_chart ?title ?width ?height drawdown_series] renders a drawdown
    series as an inverted area chart and returns a well-formed SVG 1.1 string.

    The chart draws the drawdown values as a filled area extending downward from
    a 0%% baseline. The Y-axis domain is [[min_drawdown, 0.0]]. X-axis labels
    are derived from the series index timestamps.

    The input should be a drawdown series with non-positive values, typically
    the output of {!Cairos_finance.drawdown_series}.

    @param title Optional chart title displayed above the plot area
    @param width SVG width in pixels (default [800.0])
    @param height SVG height in pixels (default [400.0]) *)
