(* Minimal cross-backend smoke test — opens a window via [Regl_backend.create_app],
   draws a single triangle, prints a few lifecycle events, and exits when
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
          fbo_num = 0;
          builtin_programs = None;
        };
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
    | _ -> m
  in
  (m', Regl_audio.silence, [])

let view (m : model) : Regl_common.renderable =
  let _ = m in
  Regl_builtin_programs.triangle
    (200., 150.)
    (600., 150.)
    (400., 450.)
    (Color.rgb 0.9 0.3 0.3)

let () =
  Printf.printf "[smoke] starting Regl_backend.create_app\n%!";
  Regl_backend.create_app init update view;
  Printf.printf "[smoke] create_app returned cleanly\n%!"
