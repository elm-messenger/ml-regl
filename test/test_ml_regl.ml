open Ml_regl_core
open Ml_regl_core.Regl_proto
open Ml_regl_js

(* Demo: exercise most public APIs with multiple scenes. *)

type sound_state = Loading | Loaded of Regl_audio.source | Failed

type scene =
  | GeometryScene
  | TextScene
  | TextureScene
  | EffectScene
  | CompositeScene
  | CameraScene
  | CustomProgramScene

type model = {
  current_ts : float;  (** absolute ms from latest Tick *)
  texture_loaded : bool;
  crop_texture_loaded : bool;
  custom_program_ready : bool;
  sound : sound_state;
  play_at : float option;  (** absolute ms; if Some, that's when to start *)
  scene : scene;
}

let initial_model =
  {
    current_ts = 0.0;
    texture_loaded = false;
    crop_texture_loaded = false;
    custom_program_ready = false;
    sound = Loading;
    play_at = None;
    scene = GeometryScene;
  }

let texture_name = "enemy"
let cropped_texture_name = "enemy-crop"
let texture_url = "/test/assets/enemy.png"
let audio_url = "/test/assets/test.ogg"
let custom_program_name = "customFlat"

let custom_program =
  {
    Regl_program.frag =
      {|
precision mediump float;
uniform vec4 color;
void main() {
  gl_FragColor = color;
}
|};
    vert =
      {|
precision mediump float;
attribute vec2 position;
void main() {
  gl_Position = vec4(position, 0.0, 1.0);
}
|};
    attributes = Some [ ("position", DynamicValue "position") ];
    uniforms = Some [ ("color", DynamicValue "color") ];
    elements = None;
    primitive = None;
    count = Some (Regl_program.static_number 3.0);
  }

let _constructor_smoke : regl_output list =
  [ load_font "demo-font" "unused.png" "unused.json" ]

let scene_of_time ts =
  match int_of_float (ts /. 1800.0) mod 7 with
  | 0 -> GeometryScene
  | 1 -> TextScene
  | 2 -> TextureScene
  | 3 -> EffectScene
  | 4 -> CompositeScene
  | 5 -> CameraScene
  | _ -> CustomProgramScene

let scene_label = function
  | GeometryScene -> "Geometry"
  | TextScene -> "Text"
  | TextureScene -> "Textures"
  | EffectScene -> "Effects"
  | CompositeScene -> "Compositors"
  | CameraScene -> "Camera"
  | CustomProgramScene -> "Custom Program"

let header_text scene =
  "Scene: " ^ scene_label scene ^ " (click to retrigger audio)"

let geometry_scene () =
  let curve =
    Regl_builtin_programs.function_curve
      (fun x -> 60.0 *. sin (x /. 50.0))
      (200., 520.) (0., 540.) 0.05 (Color.rgb 0.2 0.2 0.7)
  in
  [
    Regl_builtin_programs.triangle (120., 120.) (260., 300.) (60., 320.)
      (Color.rgb 0.9 0.3 0.3);
    Regl_builtin_programs.quad (320., 120.) (460., 120.) (460., 260.)
      (320., 260.) (Color.rgb 0.3 0.8 0.4);
    Regl_builtin_programs.rect (520., 120.) (180., 100.) (Color.rgb 0.2 0.5 0.9);
    Regl_builtin_programs.rect_centered (860., 180.) (180., 90.) 0.45
      (Color.rgba 0.9 0.6 0.2 0.9);
    Regl_builtin_programs.circle (1130., 180.) 55. (Color.rgb 0.8 0.2 0.8);
    Regl_builtin_programs.rounded_rect (1260., 120.) (220., 120.) 22.
      (Color.rgba 0.1 0.1 0.1 0.85);
    Regl_builtin_programs.poly
      [ (120., 470.); (240., 390.); (330., 430.); (300., 540.); (170., 570.) ]
      (Color.rgb 0.7 0.5 0.1);
    Regl_builtin_programs.lines
      [ ((420., 380.), (520., 520.)); ((520., 520.), (660., 410.)) ]
      (Color.rgb 0.1 0.1 0.1);
    Regl_builtin_programs.linestrip
      [ (760., 380.); (840., 420.); (900., 360.); (980., 520.); (1080., 470.) ]
      (Color.rgb 0.0 0.6 0.8);
    Regl_builtin_programs.lineloop
      [
        (1200., 380.); (1320., 410.); (1370., 520.); (1260., 570.); (1160., 500.);
      ]
      (Color.rgb 0.7 0.1 0.1);
    curve;
  ]

let text_scene scene =
  let pro_opt =
    {
      Regl_builtin_programs.default_textbox_option with
      fonts = [ "consolas"; "arial" ];
      text =
        "textbox_pro supports options like width, lineHeight,\n\
         letterSpacing, and wordBreak.";
      size = 28.0;
      color = Color.black;
      word_break = true;
      width = Some 480.0;
      line_height = Some 1.25;
      letter_spacing = Some 0.6;
      thickness = Some 0.2;
      italic = Some 0.1;
    }
  in
  [
    Regl_builtin_programs.textbox (180., 140.) 32. (header_text scene)
      "consolas" Color.black;
    Regl_builtin_programs.textbox_mf (180., 220.) 28.
      "textbox_mf with fallback font list" [ "consolas"; "arial" ]
      (Color.rgb 0.2 0.2 0.8);
    Regl_builtin_programs.textbox_centered (960., 360.) 54. "Centered Text"
      "consolas" (Color.rgb 0.8 0.2 0.2);
    Regl_builtin_programs.textbox_mf_centered (960., 500.) 30.
      "Centered multi-font text" [ "consolas"; "arial" ] Color.black;
    Regl_builtin_programs.textbox_pro (1240., 220.) pro_opt;
  ]

let texture_scene scene m =
  let fallback =
    Regl_builtin_programs.textbox_centered (960., 540.) 42.
      ("Waiting for textures in " ^ scene_label scene)
      "consolas" Color.black
  in
  if not m.texture_loaded then [ fallback ]
  else
    [
      Regl_builtin_programs.textbox (160., 110.) 30. (header_text scene)
        "consolas" Color.black;
      Regl_builtin_programs.texture (80., 220.) (320., 220.) (320., 460.)
        (80., 460.) texture_name;
      Regl_builtin_programs.texture_with_alpha (80., 500.) (320., 500.)
        (320., 740.) (80., 740.) 0.65 texture_name;
      Regl_builtin_programs.rect_texture (360., 220.) (220., 240.) texture_name;
      Regl_builtin_programs.centered_texture (760., 340.) (240., 240.) 0.4
        texture_name;
      Regl_builtin_programs.rect_texture_with_alpha (920., 220.) (220., 240.)
        0.6 texture_name;
      Regl_builtin_programs.centered_texture_with_alpha (1240., 340.)
        (240., 240.) (-0.35) 0.75 texture_name;
      Regl_builtin_programs.rect_texture_cropped (180., 520.) (240., 180.)
        (0.05, 0.05) (0.45, 0.45) texture_name;
      Regl_builtin_programs.centered_texture_cropped (620., 610.) (240., 180.)
        0.25 (0.15, 0.1) (0.5, 0.35) texture_name;
      Regl_builtin_programs.rect_texture_cropped_with_alpha (930., 520.)
        (240., 180.) (0.2, 0.15) (0.45, 0.4) 0.7 texture_name;
      Regl_builtin_programs.centered_texture_cropped_with_alpha (1260., 610.)
        (240., 180.) (-0.2) (0.1, 0.1) (0.3, 0.3) 0.85
        (if m.crop_texture_loaded then cropped_texture_name else texture_name);
      Regl_builtin_programs.texture_cropped (360., 520.) (520., 520.)
        (520., 680.) (360., 680.) (0.0, 1.0) (1.0, 1.0) (1.0, 0.0) (0.0, 0.0)
        texture_name;
      Regl_builtin_programs.texture_cropped_with_alpha (560., 520.) (720., 520.)
        (720., 680.) (560., 680.) (0.0, 1.0) (1.0, 1.0) (1.0, 0.0) (0.0, 0.0)
        0.55 texture_name;
    ]

let effect_scene scene m =
  let base =
    Regl_common.group []
      [
        Regl_builtin_programs.clear (Color.rgb 0.95 0.97 1.0);
        Regl_builtin_programs.rect (220., 180.) (420., 260.)
          (Color.rgb 0.9 0.2 0.3);
        Regl_builtin_programs.circle (840., 310.) 120. (Color.rgb 0.2 0.7 0.9);
        Regl_builtin_programs.rounded_rect (1110., 180.) (360., 220.) 24.
          (Color.rgb 0.2 0.3 0.8);
        Regl_builtin_programs.textbox_centered (960., 640.) 42.
          (header_text scene) "consolas" Color.black;
      ]
  in
  let texture_overlay =
    if m.texture_loaded then
      Regl_builtin_programs.centered_texture_with_alpha (960., 350.)
        (300., 300.) 0.0 0.35 texture_name
    else Regl_builtin_programs.empty
  in
  [
    Regl_common.group
      (Regl_effects.blur 1.5
      @ [
          Regl_effects.alpha_mult 0.9;
          Regl_effects.color_mult 0.95 0.95 1.0 1.0;
          Regl_effects.pixilation 1.2;
        ])
      [ base; texture_overlay ];
    Regl_common.group
      [
        Regl_effects.blur_h 2.0;
        Regl_effects.blur_v 2.0;
        Regl_effects.gblur_h 0.8;
        Regl_effects.gblur_v 0.8;
      ]
      [
        Regl_builtin_programs.textbox_centered (960., 740.) 30.
          "Effects: blur_h/blur_v + gblur_h/gblur_v" "consolas" Color.black;
      ];
    Regl_common.group (Regl_effects.gblur 0.8)
      [
        Regl_builtin_programs.textbox_centered (960., 820.) 32.
          "Effects: blur, alpha_mult, color_mult, outline, fxaa" "consolas"
          Color.black;
      ];
  ]

let composite_scene scene m =
  let left =
    Regl_common.group
      [ Regl_effects.color_mult 1.0 0.8 0.8 1.0 ]
      [
        Regl_builtin_programs.rect_centered (760., 420.) (420., 260.) 0.2
          (Color.rgb 0.9 0.3 0.3);
        Regl_builtin_programs.circle (900., 500.) 90. (Color.rgb 1.0 0.9 0.4);
      ]
  in
  let right =
    Regl_common.group
      [ Regl_effects.alpha_mult 0.9 ]
      [
        Regl_builtin_programs.rounded_rect (920., 260.) (380., 280.) 28.
          (Color.rgb 0.2 0.4 0.9);
        (if m.texture_loaded then
           Regl_builtin_programs.centered_texture_with_alpha (1100., 420.)
             (220., 220.) (-0.1) 0.8 texture_name
         else Regl_builtin_programs.empty);
      ]
  in
  let bottom =
    Regl_builtin_programs.textbox_centered (960., 820.) 30.
      ("Compositors: dst_over_src, linear_fade, img_fade in "
     ^ scene_label scene)
      "consolas" Color.black
  in
  [
    Regl_compositors.dst_over_src left right;
    Regl_compositors.mask_by_src left right;
    Regl_compositors.linear_fade
      ((sin (m.current_ts /. 500.0) +. 1.0) /. 2.0)
      left right;
    (if m.texture_loaded then
       Regl_compositors.img_fade texture_name
         ((cos (m.current_ts /. 700.0) +. 1.0) /. 2.0)
         false left right
     else bottom);
    bottom;
  ]

let camera_scene scene =
  let camera =
    {
      Regl_common.x = 960.0 +. (120.0 *. sin (scene_of_time 0.0 |> fun _ -> 0.0));
      y = 540.0;
      zoom = 1.1;
      rotation = 0.12;
    }
  in
  [
    Regl_common.group_with_camera camera
      (Regl_effects.gblur 0.8 @ [ Regl_effects.crt 320.0 ])
      [
        Regl_builtin_programs.rect (180., 180.) (280., 160.)
          (Color.rgb 0.8 0.2 0.2);
        Regl_builtin_programs.poly
          [ (780., 220.); (980., 150.); (1120., 280.); (900., 360.) ]
          (Color.rgb 0.2 0.8 0.5);
        Regl_builtin_programs.textbox_centered (960., 720.) 34.
          ("Camera + group_with_camera in " ^ scene_label scene)
          "consolas" Color.white;
      ];
  ]

let custom_program_scene scene m =
  let custom_renderable =
    if m.custom_program_ready then
      Regl_common.atomic custom_program_name
        [
          Regl_common.nums "position" [ -0.85; -0.25; 0.0; 0.85; 0.85; -0.25 ];
          Regl_common.nums "color" [ 0.9; 0.25; 0.65; 1.0 ];
        ]
    else
      Regl_builtin_programs.textbox_centered (960., 420.) 34.
        "Waiting for custom program..." "consolas" Color.black
  in
  [
    Regl_builtin_programs.clear (Color.rgb 0.96 0.96 0.99);
    custom_renderable;
    Regl_builtin_programs.textbox_centered (960., 780.) 30.
      ("Custom program + backend create_program in " ^ scene_label scene)
      "consolas" Color.black;
  ]

let view (m : model) =
  let scene_renderables =
    match m.scene with
    | GeometryScene -> geometry_scene ()
    | TextScene -> text_scene m.scene
    | TextureScene -> texture_scene m.scene m
    | EffectScene -> effect_scene m.scene m
    | CompositeScene -> composite_scene m.scene m
    | CameraScene -> camera_scene m.scene
    | CustomProgramScene -> custom_program_scene m.scene m
  in
  let audio_status =
    let msg =
      match m.sound with
      | Loading -> "Audio: loading..."
      | Loaded _ ->
          if m.play_at <> None then "Audio: playing (click to retrigger)"
          else "Audio: ready"
      | Failed -> "Audio: failed to load"
    in
    Regl_builtin_programs.textbox_centered (960., 1020.) 28. msg "consolas"
      Color.black
  in
  Regl_common.group [] (scene_renderables @ [ audio_status ])

let audio (m : model) : Regl_audio.audio =
  match (m.sound, m.play_at) with
  | Loaded src, Some t -> Regl_audio.audio src t
  | _ -> Regl_audio.silence

let init () =
  let startconfig : regl_start_config =
    {
      virt_width = 1920.;
      virt_height = 1080.;
      fbo_num = 5;
      builtin_programs = None;
    }
  in
  let texture_opts =
    Some
      {
        mag = Some MagNearest;
        min = Some MinLinear;
        crop = Some ((0, 0), (32, 32));
      }
  in
  ( initial_model,
    [
      start_regl startconfig;
      config_regl { time_interval = Millisecond 16.0 };
      load_texture texture_name texture_url None;
      load_texture cropped_texture_name texture_url texture_opts;
      load_audio audio_url;
      create_regl_program custom_program_name custom_program;
    ] )

let update (m : model) (e : regl_input) =
  match e with
  | Event (UpdateTick ts) ->
      let nm = { m with current_ts = ts; scene = scene_of_time ts } in
      (nm, audio nm, [])
  | Event (KeyDown _) ->
      let nm =
        match m.sound with
        | Loaded _ ->
            (* Use the latest Tick timestamp as the absolute clock. *)
            let now = m.current_ts in
            { m with play_at = Some now }
        | _ -> m
      in
      (nm, audio nm, [])
  | Event _ -> (m, audio m, [])
  | REGLRecvMsg msg ->
      let nm =
        match msg with
        | REGLTextureLoaded t when t.name = texture_name ->
            { m with texture_loaded = true }
        | REGLTextureLoaded t when t.name = cropped_texture_name ->
            { m with crop_texture_loaded = true }
        | REGLProgramCreated name when name = custom_program_name ->
            { m with custom_program_ready = true }
        | _ -> m
      in
      (nm, audio nm, [])
  | AudioMsg msg ->
      let nm =
        match msg with
        | AudioLoadSuccess { audio_url = url; source } when url = audio_url ->
            { m with sound = Loaded source }
        | AudioLoadFailed { audio_url = url; _ } when url = audio_url ->
            { m with sound = Failed }
        | _ -> m
      in
      (nm, audio nm, [])

let _ = Regl_js.create_app init update view
