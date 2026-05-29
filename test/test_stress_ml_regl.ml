open Ml_regl_core
open Ml_regl_core.Regl_proto

type model = {
  current_ts : float;
  tick_times : float list;
  texture_loaded : bool;
  triangles : Regl_common.renderable list;
  sprites : Regl_common.renderable list;
}

type scene = TrianglesOnly | SpritesOnly

let texture_name = "enemy"
let texture_url = "assets/enemy.png"

let take n xs =
  let rec aux n acc = function
    | [] -> List.rev acc
    | _ when n <= 0 -> List.rev acc
    | x :: rest -> aux (n - 1) (x :: acc) rest
  in
  aux n [] xs

let compute_fps times =
  match times with
  | [] | [ _ ] -> 0.0
  | newest :: _ ->
      let oldest = List.hd (List.rev times) in
      let frame_count = float_of_int (List.length times - 1) in
      let dt_ms = newest -. oldest in
      if dt_ms <= 0.0 then 0.0 else frame_count *. 1000.0 /. dt_ms

let build_triangles ~virt_width ~virt_height =
  let cols = 30 in
  let rows = 30 in
  let cell_w = virt_width /. float_of_int cols in
  let cell_h = virt_height /. float_of_int rows in
  let tri_in_cell i j =
    let x = float_of_int i *. cell_w in
    let y = float_of_int j *. cell_h in
    let x1 = x +. (0.12 *. cell_w) in
    let y1 = y +. (0.12 *. cell_h) in
    let x2 = x +. (0.88 *. cell_w) in
    let y2 = y +. (0.18 *. cell_h) in
    let x3 = x +. (0.25 *. cell_w) in
    let y3 = y +. (0.88 *. cell_h) in
    let r = float_of_int i /. float_of_int cols in
    let g = float_of_int j /. float_of_int rows in
    let b = 0.35 +. (0.35 *. sin (float_of_int (i + j) *. 0.6)) in
    Regl_builtin_programs.triangle (x1, y1) (x2, y2) (x3, y3)
      (Color.rgba r g b 0.9)
  in
  let rec loop_j j acc =
    if j >= rows then List.rev acc
    else
      let rec loop_i i acc =
        if i >= cols then acc else loop_i (i + 1) (tri_in_cell i j :: acc)
      in
      loop_j (j + 1) (loop_i 0 acc)
  in
  loop_j 0 []

let build_sprites ~virt_width ~virt_height =
  let cols = 18 in
  let rows = 10 in
  let cell_w = virt_width /. float_of_int cols in
  let cell_h = virt_height /. float_of_int rows in
  let size = min (cell_w *. 0.72) (cell_h *. 0.72) in
  let sprite_in_cell i j =
    let cx = (float_of_int i +. 0.5) *. cell_w in
    let cy = (float_of_int j +. 0.5) *. cell_h in
    let angle = float_of_int (i - j) *. 0.05 in
    let alpha = 0.55 +. (0.35 *. (float_of_int ((i + j) mod 7) /. 6.0)) in
    Regl_builtin_programs.centered_texture_with_alpha (cx, cy) (size, size)
      angle alpha texture_name
  in
  let rec loop_j j acc =
    if j >= rows then List.rev acc
    else
      let rec loop_i i acc =
        if i >= cols then acc else loop_i (i + 1) (sprite_in_cell i j :: acc)
      in
      loop_j (j + 1) (loop_i 0 acc)
  in
  loop_j 0 []

let initial_model ~virt_width ~virt_height =
  {
    current_ts = 0.0;
    tick_times = [];
    texture_loaded = false;
    triangles = build_triangles ~virt_width ~virt_height;
    sprites = build_sprites ~virt_width ~virt_height;
  }

let init () =
  let startconfig : regl_start_config =
    {
      virt_width = 1920.0;
      virt_height = 1080.0;
      fbo_num = 6;
      builtin_programs = None;
      window = default_window_config;
      app_name = None;
    }
  in
  let m =
    initial_model ~virt_width:startconfig.virt_width
      ~virt_height:startconfig.virt_height
  in
  ( m,
    [
      start_regl startconfig;
      config_regl (ConfigTimeInterval (Millisecond 0.01));
      load_texture texture_name texture_url None;
      load_font "consolas" "assets/Consolas.png" "assets/Consolas.json";
    ] )

let update (m : model) (e : regl_input) =
  match e with
  | Event (UpdateTick ts) ->
      let tick_times = take 10 (ts :: m.tick_times) in
      ({ m with current_ts = ts; tick_times }, Regl_audio.silence, [])
  | Event _ -> (m, Regl_audio.silence, [])
  | REGLRecvMsg msg ->
      let m =
        match msg with
        | REGLTextureLoaded t when t.name = texture_name ->
            { m with texture_loaded = true }
        | _ -> m
      in
      (m, Regl_audio.silence, [])
  | AudioMsg _ -> (m, Regl_audio.silence, [])

let view (m : model) =
  let scene =
    if int_of_float (m.current_ts /. 4000.0) mod 2 = 0 then TrianglesOnly
    else SpritesOnly
  in
  let fps = compute_fps m.tick_times in
  let tri_count = List.length m.triangles in
  let sprite_count = if m.texture_loaded then List.length m.sprites else 0 in
  let scene_label =
    match scene with TrianglesOnly -> "triangles" | SpritesOnly -> "sprites"
  in
  let fps_text =
    Printf.sprintf "fps(10): %.1f | scene=%s | triangles=%d | sprites=%d" fps
      scene_label tri_count sprite_count
  in
  let overlay =
    Regl_builtin_programs.textbox (16.0, 32.0) 26.0 fps_text "consolas"
      Color.black
  in
  let t = m.current_ts /. 1000.0 in
  let camera =
    {
      Regl_common.x = 960.0 +. (220.0 *. sin (t *. 0.7));
      y = 540.0 +. (140.0 *. cos (t *. 0.9));
      zoom = 1.0 +. (0.08 *. sin (t *. 1.3));
      rotation = 0.06 *. sin (t *. 0.8);
    }
  in
  let content =
    match scene with
    | TrianglesOnly -> m.triangles
    | SpritesOnly ->
        if m.texture_loaded then m.sprites
        else
          [
            Regl_builtin_programs.textbox_centered (960., 540.) 42.
              "loading texture..." "consolas" Color.black;
          ]
  in
  Regl_common.group []
    [
      Regl_builtin_programs.clear (Color.rgb 0.98 0.98 0.99);
      Regl_common.group_with_camera camera [] content;
      overlay;
    ]

let _ = Regl_backend.create_app init update view
