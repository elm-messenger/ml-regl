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
    ]
  in
  Printf.printf "[smoke] init: shipping %d commands\n%!" (List.length cmds);
  ({ ts = 0.0; events = 0 }, cmds)

let update (m : model) (input : regl_input) : model * Regl_audio.audio * regl_output list =
  let m' =
    match input with
    | Regl_proto.Event (Regl_proto.UpdateTick ts) ->
        { m with ts }
    | Regl_proto.Event (Regl_proto.MouseDown { button; x; y }) ->
        Printf.printf "[smoke] mouse_down b=%d x=%g y=%g\n%!" button x y;
        { m with events = m.events + 1 }
    | Regl_proto.Event (Regl_proto.MouseUp { button; x; y }) ->
        Printf.printf "[smoke] mouse_up b=%d x=%g y=%g\n%!" button x y;
        { m with events = m.events + 1 }
    | Regl_proto.Event (Regl_proto.KeyDown code) ->
        Printf.printf "[smoke] key_down %s\n%!" code;
        { m with events = m.events + 1 }
    | Regl_proto.Event (Regl_proto.KeyUp code) ->
        Printf.printf "[smoke] key_up %s\n%!" code;
        { m with events = m.events + 1 }
    | Regl_proto.REGLRecvMsg (Regl_proto.REGLTextureLoaded { name; width; height }) ->
        Printf.printf "[smoke] texture_loaded name=%s %dx%d\n%!" name width height;
        { m with events = m.events + 1 }
    | Regl_proto.REGLRecvMsg (Regl_proto.REGLTextureLoadFail name) ->
        Printf.printf "[smoke] texture_loadfail name=%s\n%!" name;
        { m with events = m.events + 1 }
    | _ -> m
  in
  (m', Regl_audio.silence, [])

let view (m : model) : Regl_common.renderable =
  (* Rotate the rect slowly so we can confirm the per-frame uniform
     uploads are working, and that the camera is being threaded. *)
  let tau = 2.0 *. Float.pi in
  let norm_angle a =
    let r = Float.rem a tau in
    if r < 0.0 then r +. tau else r
  in
  let angle = norm_angle (m.ts *. 0.001) in
  Regl_common.group [] [
    Regl_builtin_programs.clear (Color.rgb 0.6 0.7 0.6);
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
    (* M3.D Round 2: textured sprites. The "enemy" texture is
       asynchronously loaded — until the TextureLoaded event fires the
       walker silently drops these draws (matching JS). *)
    (* `rect_texture` → centeredTexture under the hood — axis-aligned. *)
    Regl_builtin_programs.rect_texture (350., 200.) (100., 100.) "enemy";
    (* `centered_texture` → rotated full-image draw. *)
    Regl_builtin_programs.centered_texture
      (700., 460.) (80., 80.) angle "enemy";
    (* `texture` → 4-corner quad (shows up to right, slanted). *)
    Regl_builtin_programs.texture
      (50., 200.) (180., 220.) (180., 320.) (40., 320.) "enemy";
    (* `centered_texture_cropped` → sample only top-left half of the
       image. (cx,cy,cw,ch) are in UV space [0,1]². *)
    Regl_builtin_programs.centered_texture_cropped
      (250., 500.) (90., 60.) angle (0., 0.) (0.5, 0.5) "enemy";
  ]

let () =
  Printf.printf "[smoke] starting Regl_backend.create_app\n%!";
  Regl_backend.create_app init update view;
  Printf.printf "[smoke] create_app returned cleanly\n%!"
