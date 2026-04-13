let margin_top = 30.0
let margin_right = 20.0
let margin_bottom = 50.0
let margin_left = 60.0
let default_width = 800.0
let default_height = 400.0

let format_timestamp (type freq) (freq : freq Cairos.Freq.t) (ts : Ptime.t) :
    string =
  let (y, m, d), ((hh, mm, _ss), _tz) = Ptime.to_date_time ts in
  match freq with
  | Cairos.Freq.Day
  | Cairos.Freq.Week ->
      Printf.sprintf "%04d-%02d-%02d" y m d
  | Cairos.Freq.Hour
  | Cairos.Freq.Minute ->
      Printf.sprintf "%04d-%02d-%02d %02d:%02d" y m d hh mm

let build_chart ~title ~width ~height ~y_domain ~y_format series ~data_scenes =
  let index = Cairos.Series.index series in
  let freq = Cairos.Index.freq index in
  let timestamps = Cairos.Index.timestamps index in
  let n = Cairos.Index.length index in
  let chart_x = margin_left in
  let chart_y = margin_top in
  let chart_width = width -. margin_left -. margin_right in
  let chart_height = height -. margin_top -. margin_bottom in
  let x_scale =
    Nopal_draw.Scale.create
      ~domain:(0.0, Float.of_int (n - 1))
      ~range:(chart_x, chart_x +. chart_width)
  in
  let y_min, y_max = y_domain in
  let y_scale =
    Nopal_draw.Scale.create ~domain:(y_min, y_max)
      ~range:(chart_y +. chart_height, chart_y)
  in
  let values = Cairos.Series.values series in
  let coords =
    Array.init n (fun i ->
        let x = Nopal_draw.Scale.apply x_scale (Float.of_int i) in
        let v = Nx.item [ i ] values in
        let y = Nopal_draw.Scale.apply y_scale v in
        (x, y))
  in
  let data = data_scenes ~y_scale coords in
  let x_config =
    {
      Nopal_charts.Axis.label = None;
      min = None;
      max = None;
      tick_count = 5;
      format_tick =
        (fun i ->
          let idx = Int.of_float (Float.round i) in
          if idx >= 0 && idx < n then format_timestamp freq timestamps.(idx)
          else "");
    }
  in
  let y_config =
    {
      Nopal_charts.Axis.label = None;
      min = None;
      max = None;
      tick_count = 5;
      format_tick = y_format;
    }
  in
  let x_ticks =
    Nopal_charts.Axis.compute_ticks x_config ~data_min:0.0
      ~data_max:(Float.of_int (n - 1))
  in
  let y_ticks =
    Nopal_charts.Axis.compute_ticks y_config ~data_min:y_min ~data_max:y_max
  in
  let x_axis =
    Nopal_charts.Axis.render_x x_config ~ticks:x_ticks ~scale:x_scale ~chart_x
      ~chart_y:(chart_y +. chart_height) ~chart_width
  in
  let y_axis =
    Nopal_charts.Axis.render_y y_config ~ticks:y_ticks ~scale:y_scale ~chart_x
      ~chart_y ~chart_height
  in
  let title_scenes =
    match title with
    | None -> []
    | Some t ->
        [
          Nopal_scene.Scene.text ~font_size:14.0
            ~font_family:Nopal_style.Font.Sans_serif
            ~font_weight:Nopal_style.Font.Bold ~anchor:Middle
            ~x:(margin_left +. (chart_width /. 2.0))
            ~y:(margin_top /. 2.0) t;
        ]
  in
  let scenes = title_scenes @ data @ x_axis @ y_axis in
  Nopal_svg.render ~width ~height scenes

let line_chart ?title ?(width = default_width) ?(height = default_height) series
    =
  let values = Cairos.Series.values series in
  let n = Cairos.Series.length series in
  let indices = Array.init n Fun.id in
  let data_min, data_max =
    Array.fold_left
      (fun (lo, hi) i ->
        let v = Nx.item [ i ] values in
        (Float.min lo v, Float.max hi v))
      (infinity, neg_infinity) indices
  in
  let color = Nopal_scene.Color.categorical.(0) in
  build_chart ~title ~width ~height ~y_domain:(data_min, data_max)
    ~y_format:(Printf.sprintf "%.2f") series
    ~data_scenes:(fun ~y_scale:_ coords ->
      let points = Array.to_list coords in
      let stroke =
        Nopal_scene.Paint.stroke ~width:2.0 (Nopal_scene.Paint.solid color)
      in
      [ Nopal_scene.Scene.polyline ~stroke points ])

let drawdown_chart ?title ?(width = default_width) ?(height = default_height)
    series =
  let values = Cairos.Series.values series in
  let n = Cairos.Series.length series in
  let indices = Array.init n Fun.id in
  let data_min =
    Array.fold_left
      (fun lo i -> Float.min lo (Nx.item [ i ] values))
      0.0 indices
  in
  let color = Nopal_scene.Color.categorical.(0) in
  let fill_color = { color with a = 0.6 } in
  build_chart ~title ~width ~height ~y_domain:(data_min, 0.0)
    ~y_format:(fun v ->
      let pct = v *. 100.0 in
      let pct = if Float.abs pct < 1e-10 then 0.0 else pct in
      Printf.sprintf "%.1f%%" pct)
    series
    ~data_scenes:(fun ~y_scale coords ->
      let zero_y = Nopal_draw.Scale.apply y_scale 0.0 in
      let data_points = Array.to_list coords in
      let baseline_points =
        List.rev_map (fun (x, _) -> (x, zero_y)) data_points
      in
      let area_points = data_points @ baseline_points in
      let segments = Nopal_draw.Path.closed_area area_points in
      let fill = Nopal_scene.Paint.solid fill_color in
      let stroke =
        Nopal_scene.Paint.stroke ~width:1.0 (Nopal_scene.Paint.solid color)
      in
      [ Nopal_scene.Scene.path ~fill ~stroke segments ])
