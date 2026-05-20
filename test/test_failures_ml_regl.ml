open Ml_regl_core
open Ml_regl_core.Regl_proto

type model = { texture_fail : bool; font_fail : bool; last_msg : string }

let missing_texture_name = "missing-texture"
let missing_font_name = "missing-font"

let init () =
  let startconfig : regl_start_config =
    {
      virt_width = 1280.0;
      virt_height = 720.0;
      fbo_num = 2;
      builtin_programs = None;
    }
  in
  ( { texture_fail = false; font_fail = false; last_msg = "loading..." },
    [
      start_regl startconfig;
      config_regl { time_interval = Millisecond 16.0 };
      load_texture missing_texture_name "/test/assets/DOES_NOT_EXIST.png" None;
      load_font missing_font_name "/test/assets/DOES_NOT_EXIST.png"
        "/test/assets/DOES_NOT_EXIST.json";
    ] )

let update (m : model) (e : regl_input) =
  match e with
  | REGLRecvMsg msg -> (
      match msg with
      | REGLTextureLoadFail name when name = missing_texture_name ->
          ( { m with texture_fail = true; last_msg = "texture load failed" },
            Regl_audio.silence,
            [] )
      | REGLFontLoadFail name when name = missing_font_name ->
          ( { m with font_fail = true; last_msg = "font load failed" },
            Regl_audio.silence,
            [] )
      | _ -> (m, Regl_audio.silence, []))
  | _ -> (m, Regl_audio.silence, [])

let view (m : model) =
  let status =
    Printf.sprintf "missing texture: %b | missing font: %b | %s" m.texture_fail
      m.font_fail m.last_msg
  in
  Regl_common.group []
    [
      Regl_builtin_programs.clear (Color.rgb 0.98 0.98 0.99);
      Regl_builtin_programs.textbox (16.0, 40.0) 30.0
        "Test: missing texture/font should fail" "consolas" Color.black;
      Regl_builtin_programs.textbox (16.0, 90.0) 26.0 status "consolas"
        Color.black;
      (if m.texture_fail && m.font_fail then
         Regl_builtin_programs.textbox (16.0, 140.0) 26.0 "PASS" "consolas"
           (Color.rgb 0.1 0.6 0.1)
       else
         Regl_builtin_programs.textbox (16.0, 140.0) 26.0 "waiting..."
           "consolas" (Color.rgb 0.7 0.4 0.0));
    ]

let _ = Ml_regl_js.Regl_js.create_app init update view
