(* Desktop-only smoke test for window config.

   Exercises both window-config entry points: - [StartRegl.window]: the initial
   window flags. We bring the window up explicitly NON-resizable so the very
   first frame already reflects the [WindowConfig] in StartRegl. - [ConfigRegl]
   / [ConfigWindow]: runtime overrides. We then flip resizable back on, toggle
   fullscreen on, toggle it off, and finally quit.

   This test is visual: drag the window edge during each phase to confirm the
   resizable flag, and look at the screen during the fullscreen phase to confirm
   it covers the display. The on-screen text reports which phase is active so
   it's obvious at a glance. *)

open Ml_regl_core
open Ml_regl_core.Regl_proto

type phase =
  | P0_NonResizable
  | P1_ResizableOn
  | P2_FullscreenOn
  | P3_FullscreenOff
  | P4_Quitting

let phase_label = function
  | P0_NonResizable -> "phase 0: non-resizable (initial StartRegl.window)"
  | P1_ResizableOn -> "phase 1: resizable=true via ConfigWindow"
  | P2_FullscreenOn -> "phase 2: fullscreen=true via ConfigWindow"
  | P3_FullscreenOff -> "phase 3: fullscreen=false via ConfigWindow"
  | P4_Quitting -> "phase 4: quit_regl shipped"

let virt_w = 1280.0
let virt_h = 720.0

(* Frame markers between phase transitions. Wide enough that you can actually
   try to drag the edge / observe the fullscreen flip manually. At 60 fps these
   are ~2 s apart. *)
let frame_p1 = 120
let frame_p2 = 240
let frame_p3 = 360
let frame_quit = 480

type model = { ts : float; frame : int; phase : phase }

let init () : model * regl_output list =
  let cmds =
    [
      start_regl
        {
          virt_width = virt_w;
          virt_height = virt_h;
          fbo_num = 2;
          builtin_programs = None;
          (* StartRegl.window: open the window non-resizable from the very first
             frame. fullscreen left at None = use the platform default
             (windowed). *)
          window = { fullscreen = None; resizable = Some false };
          app_name = None;
        };
      config_regl (ConfigTimeInterval AnimationFrame);
      load_font "consolas" "test/assets/Consolas.png"
        "test/assets/Consolas.json";
    ]
  in
  ({ ts = 0.0; frame = 0; phase = P0_NonResizable }, cmds)

let advance_phase (m : model) : model * regl_output list =
  let next_at, next_phase, cmds =
    match m.phase with
    | P0_NonResizable ->
        ( frame_p1,
          P1_ResizableOn,
          [
            config_regl
              (ConfigWindow { fullscreen = None; resizable = Some true });
          ] )
    | P1_ResizableOn ->
        ( frame_p2,
          P2_FullscreenOn,
          [
            config_regl
              (ConfigWindow { fullscreen = Some true; resizable = None });
          ] )
    | P2_FullscreenOn ->
        ( frame_p3,
          P3_FullscreenOff,
          [
            config_regl
              (ConfigWindow { fullscreen = Some false; resizable = None });
          ] )
    | P3_FullscreenOff -> (frame_quit, P4_Quitting, [ quit_regl () ])
    | P4_Quitting -> (max_int, P4_Quitting, [])
  in
  if m.frame = next_at && next_phase <> m.phase then
    ({ m with phase = next_phase }, cmds)
  else (m, [])

let update (m : model) (input : regl_input) :
    model * Regl_audio.audio * regl_output list =
  let m', extra_cmds =
    match input with
    | Regl_proto.Event (Regl_proto.UpdateTick ts) ->
        let m = { m with ts; frame = m.frame + 1 } in
        advance_phase m
    | _ -> (m, [])
  in
  (m', Regl_audio.silence, extra_cmds)

let view (m : model) : Regl_common.renderable =
  (* Pick a per-phase background tint so phase transitions are visible even on a
     tiny window. *)
  let bg_color =
    match m.phase with
    | P0_NonResizable -> Color.rgb 0.10 0.10 0.18
    | P1_ResizableOn -> Color.rgb 0.10 0.20 0.10
    | P2_FullscreenOn -> Color.rgb 0.20 0.10 0.10
    | P3_FullscreenOff -> Color.rgb 0.10 0.18 0.20
    | P4_Quitting -> Color.rgb 0.05 0.05 0.05
  in
  Regl_common.group []
    [
      Regl_builtin_programs.clear bg_color;
      Regl_builtin_programs.textbox (20., 40.) 22.0 (phase_label m.phase)
        "consolas" (Color.rgb 1.0 1.0 1.0);
      Regl_builtin_programs.textbox (20., 90.) 18.0
        (Printf.sprintf "frame=%d  ts=%.0fms" m.frame m.ts)
        "consolas" (Color.rgb 0.85 0.85 0.85);
      Regl_builtin_programs.textbox (20., 130.) 16.0
        "drag the window edge to verify resizable; watch for fullscreen flip"
        "consolas" (Color.rgb 0.7 0.7 0.7);
    ]

let () = Regl_backend.create_app init update view
