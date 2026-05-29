(* Cross-backend textbox smoke test.

   Exercises the frontend textbox API and the desktop textbox renderer with: -
   explicit multiline strings, - width-based wrapping with [word_break = false],
   - forced character wrapping with [word_break = true], - italic skew via
   [italic], - bold/MSDF weight via [thickness], - centered/right/bottom
   alignment and spacing options.

   The bundled Consolas MSDF atlas is intentionally small, so all visible test
   strings stay lowercase/digits/basic punctuation to avoid missing glyphs. *)

open Ml_regl_core
open Ml_regl_core.Regl_proto

type model = { ts : float; frame : int; font_loaded : bool }

let virt_w = 1280.0
let virt_h = 720.0
let font_name = "consolas"
let font_png = "test/assets/Consolas.png"
let font_json = "test/assets/Consolas.json"

let init () : model * regl_output list =
  let cmds =
    [
      start_regl
        {
          virt_width = virt_w;
          virt_height = virt_h;
          fbo_num = 4;
          builtin_programs = None;
          window = default_window_config;
          app_name = None;
        };
      config_regl (ConfigTimeInterval AnimationFrame);
      load_font font_name font_png font_json;
    ]
  in
  ({ ts = 0.0; frame = 0; font_loaded = false }, cmds)

let update (m : model) (input : regl_input) :
    model * Regl_audio.audio * regl_output list =
  let m, cmds =
    match input with
    | Event (UpdateTick ts) ->
        let frame = m.frame + 1 in
        ({ m with ts; frame }, [])
    | REGLRecvMsg (REGLFontLoaded name) when name = font_name ->
        ({ m with font_loaded = true }, [])
    | REGLRecvMsg (REGLFontLoadFail { name; _ }) when name = font_name -> (m, [])
    | _ -> (m, [])
  in
  (m, Regl_audio.silence, cmds)

let panel (x, y) (w, h) color = Regl_builtin_programs.rect (x, y) (w, h) color

let label (x, y) text =
  Regl_builtin_programs.textbox (x, y) 15.0 text font_name
    (Color.rgb 0.65 0.7 0.78)

let textbox_box (x, y) (w, h) =
  Regl_common.group []
    [
      panel (x, y) (w, h) (Color.rgba 0.10 0.12 0.17 0.92);
      panel (x, y) (w, 2.0) (Color.rgb 0.28 0.33 0.44);
      panel (x, y +. h -. 2.0) (w, 2.0) (Color.rgb 0.28 0.33 0.44);
      panel (x, y) (2.0, h) (Color.rgb 0.28 0.33 0.44);
      panel (x +. w -. 2.0, y) (2.0, h) (Color.rgb 0.28 0.33 0.44);
    ]

let multiline_case =
  Regl_builtin_programs.textbox_pro (36.0, 92.0)
    {
      Regl_builtin_programs.default_textbox_option with
      fonts = [ font_name ];
      text = "line one\nline two\nline three";
      size = 24.0;
      color = Color.rgb 0.96 0.96 0.98;
      line_height = Some 1.25;
      letter_spacing = Some 0.2;
    }

let word_wrap_case =
  Regl_builtin_programs.textbox_pro (360.0, 92.0)
    {
      Regl_builtin_programs.default_textbox_option with
      fonts = [ font_name ];
      text = "word wrap keeps words together inside a narrow box";
      size = 22.0;
      color = Color.rgb 0.70 0.92 1.0;
      width = Some 230.0;
      line_height = Some 1.18;
      word_spacing = Some 1.15;
      word_break = false;
    }

let word_break_case =
  Regl_builtin_programs.textbox_pro (690.0, 92.0)
    {
      Regl_builtin_programs.default_textbox_option with
      fonts = [ font_name ];
      text = "supercalifragilisticexpialidocious";
      size = 21.0;
      color = Color.rgb 1.0 0.82 0.50;
      width = Some 155.0;
      line_height = Some 1.12;
      word_break = true;
    }

let italic_bold_case t =
  let wobble = 10.0 *. sin (t *. 0.0025) in
  Regl_builtin_programs.textbox_pro (46.0, 338.0)
    {
      Regl_builtin_programs.default_textbox_option with
      fonts = [ font_name ];
      text = "italic skew and bold thickness";
      size = 28.0;
      color = Color.rgb 1.0 0.90 0.35;
      italic = Some wobble;
      thickness = Some 0.5;
      letter_spacing = Some 0.4;
    }

let centered_case =
  Regl_builtin_programs.textbox_pro (505.0, 532.0)
    {
      Regl_builtin_programs.default_textbox_option with
      fonts = [ font_name ];
      text = "center\naligned\nmultiline";
      size = 27.0;
      color = Color.rgb 0.80 1.0 0.72;
      align = Some "center";
      valign = Some "center";
      line_height = Some 1.15;
      thickness = Some 0.12;
    }

let right_bottom_case =
  Regl_builtin_programs.textbox_pro (940.0, 594.0)
    {
      Regl_builtin_programs.default_textbox_option with
      fonts = [ font_name ];
      text = "right bottom\nvalign test";
      size = 24.0;
      color = Color.rgb 1.0 0.72 0.88;
      align = Some "right";
      valign = Some "bottom";
      line_height = Some 1.2;
    }

let view (m : model) : Regl_common.renderable =
  let status =
    if m.font_loaded then "font loaded" else "waiting for font load"
  in
  Regl_common.group []
    [
      Regl_builtin_programs.clear (Color.rgb 0.04 0.05 0.075);
      textbox_box (24.0, 64.0) (280.0, 170.0);
      textbox_box (344.0, 64.0) (280.0, 170.0);
      textbox_box (674.0, 64.0) (280.0, 170.0);
      textbox_box (24.0, 300.0) (930.0, 92.0);
      textbox_box (344.0, 442.0) (322.0, 180.0);
      textbox_box (674.0, 442.0) (280.0, 180.0);
      label (24.0, 28.0)
        (Printf.sprintf "textbox smoke | %s | frame=%d" status m.frame);
      label (36.0, 56.0) "multiline newline";
      label (360.0, 56.0) "word wrap";
      label (690.0, 56.0) "wordbreak long word";
      label (46.0, 292.0) "italic + bold thickness (animated skew)";
      label (360.0, 434.0) "centered align/valign";
      label (688.0, 434.0) "right + bottom align";
      multiline_case;
      word_wrap_case;
      word_break_case;
      italic_bold_case m.ts;
      centered_case;
      right_bottom_case;
    ]

let () = Regl_backend.create_app init update view
