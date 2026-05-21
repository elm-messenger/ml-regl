(* Cross-backend smoke test — opens a window via [Regl_backend.create_app],
   draws one of every M3.C 2D primitive (triangle, rect, circle,
   roundedRect, clear), prints a few lifecycle events, and exits when
   the window is closed. Same source compiles for either backend; the
   choice is made in the dune executable stanza via the (libraries ...)
   field. *)

open Ml_regl_core
open Ml_regl_core.Regl_proto

type model = {
  ts : float;
  events : int;
  frame : int;
  late_load_shipped : bool;
}

let virt_w = 800.0
let virt_h = 600.0

let init () : model * regl_output list =
  let cmds =
    [
      config_regl { time_interval = AnimationFrame };
      start_regl
        {
          virt_width = virt_w;
          virt_height = virt_h;
          fbo_num = 5;
          builtin_programs = None;
        };
      (* M3.D Round 1: ship a LoadTexture and observe the round-trip
         BackendEvent in [update] below. URL is filesystem-relative to
         the working directory the binary is launched from. *)
      load_texture "enemy" "test/assets/enemy.png" None;
      (* M3.F: ship a LoadFont. The desktop backend reads the JSON
         metrics off disk, decodes the atlas PNG, registers a Font and
         a Texture, and ships back a [REGLFontLoaded] event. The
         walker's [textbox] branch silently no-ops on draws referencing
         this font until the load completes (mirrors the JS backend's
         first-frame async-load tolerance). *)
      load_font "custom" "test/assets/custom.png" "test/assets/custom-msdf.json";
    ]
  in
  Printf.printf "[smoke] init: shipping %d commands\n%!" (List.length cmds);
  ({ ts = 0.0; events = 0; frame = 0; late_load_shipped = false }, cmds)

let update (m : model) (input : regl_input) : model * Regl_audio.audio * regl_output list =
  let m', extra_cmds =
    match input with
    | Regl_proto.Event (Regl_proto.UpdateTick ts) ->
        let frame' = m.frame + 1 in
        (* M3.F.2 async-load exercise: ~1 second into the run, ship a
           second LoadTexture mid-game to verify the worker-thread
           pipeline and the GL-thread drain both work outside of the
           init burst. The walker's silent-drop on missing textures
           covers the gap before [texture_loaded] arrives. *)
        if frame' = 60 && not m.late_load_shipped then
          ({ m with ts; frame = frame'; late_load_shipped = true },
           [ load_texture "enemy_late" "test/assets/enemy.png" None ])
        else
          ({ m with ts; frame = frame' }, [])
    | Regl_proto.Event (Regl_proto.MouseDown { button; x; y }) ->
        Printf.printf "[smoke] mouse_down b=%d x=%g y=%g\n%!" button x y;
        ({ m with events = m.events + 1 }, [])
    | Regl_proto.Event (Regl_proto.MouseUp { button; x; y }) ->
        Printf.printf "[smoke] mouse_up b=%d x=%g y=%g\n%!" button x y;
        ({ m with events = m.events + 1 }, [])
    | Regl_proto.Event (Regl_proto.KeyDown code) ->
        Printf.printf "[smoke] key_down %s\n%!" code;
        ({ m with events = m.events + 1 }, [])
    | Regl_proto.Event (Regl_proto.KeyUp code) ->
        Printf.printf "[smoke] key_up %s\n%!" code;
        ({ m with events = m.events + 1 }, [])
    | Regl_proto.REGLRecvMsg (Regl_proto.REGLTextureLoaded { name; width; height }) ->
        Printf.printf "[smoke] texture_loaded name=%s %dx%d\n%!" name width height;
        ({ m with events = m.events + 1 }, [])
    | Regl_proto.REGLRecvMsg (Regl_proto.REGLTextureLoadFail name) ->
        Printf.printf "[smoke] texture_loadfail name=%s\n%!" name;
        ({ m with events = m.events + 1 }, [])
    | Regl_proto.REGLRecvMsg (Regl_proto.REGLFontLoaded name) ->
        Printf.printf "[smoke] font_loaded name=%s\n%!" name;
        ({ m with events = m.events + 1 }, [])
    | Regl_proto.REGLRecvMsg (Regl_proto.REGLFontLoadFail name) ->
        Printf.printf "[smoke] font_loadfail name=%s\n%!" name;
        ({ m with events = m.events + 1 }, [])
    | _ -> (m, [])
  in
  (m', Regl_audio.silence, extra_cmds)

let view (m : model) : Regl_common.renderable =
  (* Rotate the rect slowly so we can confirm the per-frame uniform
     uploads are working, and that the camera is being threaded. *)
  let tau = 2.0 *. Float.pi in
  let norm_angle a =
    let r = Float.rem a tau in
    if r < 0.0 then r +. tau else r
  in
  let angle = norm_angle (m.ts *. 0.001) in
  let bg =
    Regl_common.group [] [
      Regl_builtin_programs.clear (Color.rgb 0.05 0.07 0.10);
      (* Filled triangle (uses caller-supplied pos). *)
      Regl_builtin_programs.triangle
        (200., 150.) (600., 150.) (400., 450.)
        (Color.rgb 0.9 0.3 0.3);
      (* Axis-aligned rect (uses hardcoded [0,1]² quad + posize). *)
      Regl_builtin_programs.rect (60., 60.) (120., 80.)
        (Color.rgb 0.2 0.6 0.9);
      (* Rotated centered rect. *)
      Regl_builtin_programs.rect_centered (660., 100.) (120., 60.) angle
        (Color.rgb 0.95 0.85 0.2);
      (* Filled circle (uses NDC fullscreen quad + cr SDF). *)
      Regl_builtin_programs.circle (140., 480.) 60.
        (Color.rgb 0.3 0.9 0.4);
      (* Rounded rect (uses NDC fullscreen quad + cs+radius SDF). *)
      Regl_builtin_programs.rounded_rect (520., 460.) (200., 100.) 22.
        (Color.rgb 0.85 0.4 0.95);
    ]
  in
  let sprites =
    Regl_common.group [] [
      Regl_builtin_programs.rect_texture (350., 200.) (100., 100.) "enemy";
      Regl_builtin_programs.centered_texture
        (700., 460.) (80., 80.) angle "enemy";
      Regl_builtin_programs.texture
        (50., 200.) (180., 220.) (180., 320.) (40., 320.) "enemy";
      Regl_builtin_programs.centered_texture_cropped
        (250., 500.) (90., 60.) angle (0., 0.) (0.5, 0.5) "enemy";
      (* Mid-run async-loaded copy. The walker silently drops this draw
         until [texture_loaded "enemy_late"] arrives ~1 second in. *)
      Regl_builtin_programs.rect_texture (470., 200.) (100., 100.) "enemy_late";
    ]
  in
  (* M3.E sanity tests:
     1. [color_mult] effect tints the sprites red — proves
        the effect path (single-pass FBO ping-pong) works.
     2. [linear_fade] composite fades from `bg` to `sprites_tinted`
        based on a slow sin wave — proves composite + sampler binding
        for both halves works. *)
  let fade_t = 0.5 +. 0.5 *. sin (m.ts *. 0.0008) in
  let sprites_tinted =
    Regl_common.group
      [ Regl_effects.color_mult 1.0 0.4 0.4 1.0 ]
      [ sprites ]
  in
    Regl_common.group
      [ ]
      [ Regl_builtin_programs.clear (Color.white);
        Regl_compositors.linear_fade fade_t bg sprites_tinted;
        (* M3.F: a few textbox draws. The atlas only carries lowercase +
           digits + symbols, so we keep the strings lowercase. The first
           is left-aligned; the second uses [textbox_centered] to verify
           the centered align/valign math; the third uses [textbox_pro]
           with non-default options (size/wordSpacing/letterSpacing/
           color) to exercise the field-pull path. *)
        Regl_builtin_programs.textbox
          (40., 40.)
          24.0
          "hello world"
          "custom"
          (Color.rgb 1.0 1.0 1.0);
        Regl_builtin_programs.textbox_centered
          (400., 300.)
          48.0
          "ml-regl"
          "custom"
          (Color.rgb 0.95 0.95 0.2);
        Regl_builtin_programs.textbox_pro (40., 540.)
          { Regl_builtin_programs.default_textbox_option with
            fonts = [ "custom" ];
            text  = "the quick brown fox jumps over 13 lazy dogs!";
            size  = 18.0;
            color = Color.rgb 0.5 0.9 1.0;
            letter_spacing = Some 0.5;
            word_spacing   = Some 1.0;
          };
      ]

let () =
  Printf.printf "[smoke] starting Regl_backend.create_app\n%!";
  Regl_backend.create_app init update view;
  Printf.printf "[smoke] create_app returned cleanly\n%!"
