open Ml_regl
open Js_of_ocaml

type model = {
  texture_fail : bool;
  font_fail : bool;
  last_msg : string;
}

let missing_texture_name = "missing-texture"
let missing_font_name = "missing-font"

let init (canvas : Dom_html.canvasElement Js.t option) =
  let startconfig : Regl.regl_start_config =
    {
      virt_width = 1280.0;
      virt_height = 720.0;
      fbo_num = 2;
      builtin_programs = None;
    }
  in
  let mc = Option.get canvas in
  mc##.width := 1280;
  mc##.height := 720;
  ( { texture_fail = false; font_fail = false; last_msg = "loading..." },
    [
      Regl.start_regl startconfig;
      Regl.config_regl { time_interval = Millisecond 16.0 };
      Regl.load_texture missing_texture_name "/test/assets/DOES_NOT_EXIST.png" None;
      Regl.load_font missing_font_name "/test/assets/DOES_NOT_EXIST.png"
        "/test/assets/DOES_NOT_EXIST.json";
    ] )

let update (_canvas : Dom_html.canvasElement Js.t option) (m : model)
    (e : Regl.regl_input) =
  match e with
  | Regl.REGLRecvMsg msg -> (
      match msg with
      | Regl.REGLTextureLoadFail name when name = missing_texture_name ->
          ( { m with texture_fail = true; last_msg = "texture load failed" },
            Regl_audio.silence,
            [] )
      | Regl.REGLFontLoadFail name when name = missing_font_name ->
          ( { m with font_fail = true; last_msg = "font load failed" },
            Regl_audio.silence,
            [] )
      | _ -> (m, Regl_audio.silence, []))
  | _ -> (m, Regl_audio.silence, [])

let view (m : model) =
  let status =
    Printf.sprintf "missing texture: %b | missing font: %b | %s"
      m.texture_fail m.font_fail m.last_msg
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

let _ = Regl.create_app init update view

