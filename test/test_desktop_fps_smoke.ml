(* Desktop-only smoke test for runtime frame-rate changes.

   Walks through several pacing configs by shipping [config_regl
   (ConfigTimeInterval ...)] mid-run and measures the actual achieved frame
   interval over a sliding window. The on-screen text shows the requested target
   vs the measured average so the pacing change is obvious at a glance. *)

open Ml_regl_core
open Ml_regl_core.Regl_proto

type phase =
  | P0_Vsync (* AnimationFrame: vsync / adaptive vsync *)
  | P1_Slow (* Millisecond 100. -> ~10 fps *)
  | P2_Medium (* Millisecond 33.33 -> ~30 fps *)
  | P3_Fast (* Millisecond 8. -> ~125 fps cap *)
  | P4_BackToVsync (* AnimationFrame again *)
  | P5_Quitting

let phase_label = function
  | P0_Vsync -> "phase 0: AnimationFrame (vsync)"
  | P1_Slow -> "phase 1: Millisecond 100  (~10 fps)"
  | P2_Medium -> "phase 2: Millisecond 33.33 (~30 fps)"
  | P3_Fast -> "phase 3: Millisecond 8 (~125 fps)"
  | P4_BackToVsync -> "phase 4: AnimationFrame (vsync, restored)"
  | P5_Quitting -> "phase 5: quit_regl shipped"

(* Phase boundaries in *real* milliseconds (UpdateTick.ts), not frames — at
   variable frame rates a frame counter would lie. Each phase lasts ~3 s so the
   measured rate has time to settle. *)
let t_p1 = 3_000.0
let t_p2 = 6_000.0
let t_p3 = 9_000.0
let t_p4 = 12_000.0
let t_quit = 15_000.0
let virt_w = 800.0
let virt_h = 600.0

(* Sliding-window history of recent frame timestamps, used to compute the
   achieved rate. *)
let history_size = 60

type model = {
  ts : float;
  frame : int;
  phase : phase;
  history : float list; (* most-recent UpdateTick.ts values, head = newest *)
}

let init () : model * regl_output list =
  let cmds =
    [
      start_regl
        {
          virt_width = virt_w;
          virt_height = virt_h;
          fbo_num = 2;
          builtin_programs = None;
          window = default_window_config;
        };
      config_regl (ConfigTimeInterval AnimationFrame);
      load_font "custom" "test/assets/custom.png" "test/assets/custom-msdf.json";
    ]
  in
  ({ ts = 0.0; frame = 0; phase = P0_Vsync; history = [] }, cmds)

(* Push a new sample, drop the oldest if over capacity. *)
let push_history ts hist =
  let h = ts :: hist in
  let rec take n = function
    | _ when n = 0 -> []
    | [] -> []
    | x :: rest -> x :: take (n - 1) rest
  in
  take history_size h

(* Achieved fps over [history]: (n - 1) / span_seconds. Returns 0. when there
   aren't enough samples yet. *)
let measured_fps hist =
  match hist with
  | [] | [ _ ] -> 0.0
  | newest :: _ ->
      let oldest = List.nth hist (List.length hist - 1) in
      let span_s = (newest -. oldest) /. 1000.0 in
      if span_s <= 0.0 then 0.0
      else float_of_int (List.length hist - 1) /. span_s

let advance_phase (m : model) : model * regl_output list =
  let next_at, next_phase, cmds =
    match m.phase with
    | P0_Vsync ->
        (t_p1, P1_Slow, [ config_regl (ConfigTimeInterval (Millisecond 100.0)) ])
    | P1_Slow ->
        ( t_p2,
          P2_Medium,
          [ config_regl (ConfigTimeInterval (Millisecond 33.333)) ] )
    | P2_Medium ->
        (t_p3, P3_Fast, [ config_regl (ConfigTimeInterval (Millisecond 8.0)) ])
    | P3_Fast ->
        ( t_p4,
          P4_BackToVsync,
          [ config_regl (ConfigTimeInterval AnimationFrame) ] )
    | P4_BackToVsync -> (t_quit, P5_Quitting, [ quit_regl () ])
    | P5_Quitting -> (Float.infinity, P5_Quitting, [])
  in
  if m.ts >= next_at && next_phase <> m.phase then
    (* Reset history so the post-transition measurement isn't contaminated by
       the previous phase's interval. *)
    ({ m with phase = next_phase; history = [] }, cmds)
  else (m, [])

let update (m : model) (input : regl_input) :
    model * Regl_audio.audio * regl_output list =
  let m', extra_cmds =
    match input with
    | Regl_proto.Event (Regl_proto.UpdateTick ts) ->
        let m =
          {
            m with
            ts;
            frame = m.frame + 1;
            history = push_history ts m.history;
          }
        in
        advance_phase m
    | _ -> (m, [])
  in
  (m', Regl_audio.silence, extra_cmds)

let view (m : model) : Regl_common.renderable =
  let bg_color =
    match m.phase with
    | P0_Vsync -> Color.rgb 0.10 0.10 0.18
    | P1_Slow -> Color.rgb 0.18 0.10 0.10
    | P2_Medium -> Color.rgb 0.18 0.14 0.05
    | P3_Fast -> Color.rgb 0.05 0.18 0.10
    | P4_BackToVsync -> Color.rgb 0.10 0.18 0.18
    | P5_Quitting -> Color.rgb 0.05 0.05 0.05
  in
  (* A spinning rect provides a visual rate cue independent of the numbers — at
     10 fps it is visibly steppy, at vsync it's smooth. *)
  let tau = 2.0 *. Float.pi in
  let angle = Float.rem (m.ts *. 0.002) tau in
  let fps = measured_fps m.history in
  Regl_common.group []
    [
      Regl_builtin_programs.clear bg_color;
      Regl_builtin_programs.rect_centered (400., 360.) (160., 90.) angle
        (Color.rgb 0.95 0.85 0.2);
      Regl_builtin_programs.textbox (20., 40.) 22.0 (phase_label m.phase)
        "custom" (Color.rgb 1.0 1.0 1.0);
      Regl_builtin_programs.textbox (20., 90.) 18.0
        (Printf.sprintf "measured: %.2f fps  (frame=%d  t=%.0fms)" fps m.frame
           m.ts)
        "custom" (Color.rgb 0.85 0.95 0.85);
      Regl_builtin_programs.textbox (20., 130.) 16.0
        "watch the spinning rect: smooth at vsync, steppy at 10 fps" "custom"
        (Color.rgb 0.7 0.7 0.7);
    ]

let () = Regl_backend.create_app init update view
