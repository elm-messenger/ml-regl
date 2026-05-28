(* Cross-backend smoke test for mouse event coordinates.

   Purpose: verify that MouseDown / MouseUp / MouseMove events delivered to
   OCaml are in StartRegl virtual coordinates, not raw host window/page pixels.

   Manual check: - Run the executable/page, then move/click inside it. - The
   on-screen coordinate readout and cursor marker should stay in the virtual
   400x300 canvas range regardless of host presentation size / high-DPI scaling.
   - A click in the visual centre should report roughly x=200, y=150. - Mouse
   buttons should use SDL numbering: left=1, middle=2, right=3.

   The test also performs runtime assertions for any received mouse event: if a
   coordinate escapes the virtual canvas bounds (with tiny tolerance), it shows
   the failure on screen and exits non-zero after the window closes. *)

open Ml_regl_core
open Ml_regl_core.Regl_proto

let virt_w = 1920.0
let virt_h = 1200.0
let quit_after_ms = 20_000.0
let eps = 0.001
let had_failure = ref false

type mouse_kind = Move | Down | Up

type mouse_sample = {
  kind : mouse_kind;
  button : int option;
  x : float;
  y : float;
}

type model = {
  ts : float;
  frame : int;
  last : mouse_sample option;
  move_count : int;
  down_count : int;
  up_count : int;
  failures : string list;
  quitting : bool;
}

let kind_label = function Move -> "move" | Down -> "down" | Up -> "up"

let sample_label = function
  | None -> "no mouse event yet"
  | Some s -> (
      match s.button with
      | None -> Printf.sprintf "%s x=%.2f y=%.2f" (kind_label s.kind) s.x s.y
      | Some b ->
          Printf.sprintf "%s button=%d x=%.2f y=%.2f" (kind_label s.kind) b s.x
            s.y)

let in_virtual_bounds x y =
  x >= -.eps && x <= virt_w +. eps && y >= -.eps && y <= virt_h +. eps

let validate_sample s failures =
  let failures =
    if in_virtual_bounds s.x s.y then failures
    else
      Printf.sprintf "OUT_OF_RANGE %s x=%.3f y=%.3f (expected 0..%.0f, 0..%.0f)"
        (kind_label s.kind) s.x s.y virt_w virt_h
      :: failures
  in
  match s.button with
  | Some b when b < 1 || b > 5 ->
      Printf.sprintf "BAD_BUTTON %s button=%d (expected SDL 1..5)"
        (kind_label s.kind) b
      :: failures
  | _ -> failures

let record_sample m sample =
  let failures = validate_sample sample m.failures in
  if List.length failures > List.length m.failures then had_failure := true;
  {
    m with
    last = Some sample;
    failures;
    move_count = (m.move_count + if sample.kind = Move then 1 else 0);
    down_count = (m.down_count + if sample.kind = Down then 1 else 0);
    up_count = (m.up_count + if sample.kind = Up then 1 else 0);
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
          window = { fullscreen = None; resizable = Some true };
        };
      config_regl (ConfigTimeInterval AnimationFrame);
      load_font "consolas" "test/assets/Consolas.png"
        "test/assets/Consolas.json";
    ]
  in
  ( {
      ts = 0.0;
      frame = 0;
      last = None;
      move_count = 0;
      down_count = 0;
      up_count = 0;
      failures = [];
      quitting = false;
    },
    cmds )

let maybe_quit m =
  if (not m.quitting) && m.ts >= quit_after_ms then
    ({ m with quitting = true }, [ quit_regl () ])
  else (m, [])

let update (m : model) (input : regl_input) :
    model * Regl_audio.audio * regl_output list =
  let m =
    match input with
    | Event (UpdateTick ts) -> { m with ts; frame = m.frame + 1 }
    | Event (MouseMove { x; y }) ->
        record_sample m { kind = Move; button = None; x; y }
    | Event (MouseDown { button; x; y }) ->
        record_sample m { kind = Down; button = Some button; x; y }
    | Event (MouseUp { button; x; y }) ->
        record_sample m { kind = Up; button = Some button; x; y }
    | _ -> m
  in
  let m, cmds = maybe_quit m in
  (m, Regl_audio.silence, cmds)

let small_text x y text color =
  Regl_builtin_programs.textbox (x, y) 14.0 text "consolas" color

let view (m : model) : Regl_common.renderable =
  let ok = m.failures = [] in
  let bg = if ok then Color.rgb 0.06 0.08 0.10 else Color.rgb 0.22 0.04 0.04 in
  let fg = Color.rgb 0.95 0.95 0.95 in
  let muted = Color.rgb 0.68 0.74 0.78 in
  let accent =
    if ok then Color.rgb 0.20 0.90 0.45 else Color.rgb 1.0 0.25 0.18
  in
  let cursor_shapes =
    match m.last with
    | None -> []
    | Some s ->
        [
          Regl_builtin_programs.circle (s.x, s.y) 5.0 accent;
          Regl_builtin_programs.rect
            (s.x -. 12.0, s.y -. 1.0)
            (24.0, 2.0) accent;
          Regl_builtin_programs.rect
            (s.x -. 1.0, s.y -. 12.0)
            (2.0, 24.0) accent;
        ]
  in
  let failure_lines =
    m.failures |> List.rev
    |> List.mapi (fun i msg ->
        small_text 18.0
          (206.0 +. (float i *. 18.0))
          msg (Color.rgb 1.0 0.55 0.45))
  in
  Regl_common.group []
    ([
       Regl_builtin_programs.clear bg;
       (* Visual virtual-canvas frame and centre marker. *)
       Regl_builtin_programs.rect (0.0, 0.0) (virt_w, 2.0) accent;
       Regl_builtin_programs.rect (0.0, virt_h -. 2.0) (virt_w, 2.0) accent;
       Regl_builtin_programs.rect (0.0, 0.0) (2.0, virt_h) accent;
       Regl_builtin_programs.rect (virt_w -. 2.0, 0.0) (2.0, virt_h) accent;
       Regl_builtin_programs.circle
         (virt_w /. 2.0, virt_h /. 2.0)
         3.0 (Color.rgb 0.9 0.8 0.2);
       small_text 18.0 22.0 "mouse event virtual-coordinate smoke test" fg;
       small_text 18.0 46.0
         "resize the window, then move/click: coords should stay within 0..400 \
          / 0..300"
         muted;
       small_text 18.0 70.0
         "left/middle/right buttons should report 1/2/3 (SDL convention)" muted;
       small_text 18.0 104.0 (sample_label m.last) fg;
       small_text 18.0 128.0
         (Printf.sprintf "counts: move=%d down=%d up=%d   t=%.1fs   failures=%d"
            m.move_count m.down_count m.up_count (m.ts /. 1000.0)
            (List.length m.failures))
         fg;
       small_text 18.0 162.0
         (if ok then "status: OK" else "status: FAILED -- see lines below")
         accent;
       small_text 18.0 184.0 "auto-quit at 20s; close window anytime" muted;
     ]
    @ cursor_shapes @ failure_lines)

let () =
  Regl_backend.create_app init update view;
  if !had_failure then exit 1
