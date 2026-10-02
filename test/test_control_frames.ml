(* Cross-backend app for control-protocol checks: the clear colour changes
   on every update, so a screenshot shows which frame's view it holds. The
   window is 1280x800 and fixed, like a game's. test/native_control_smoke.py
   drives the desktop build. *)

open Ml_regl_core
open Ml_regl_core.Regl_proto

let colors =
  [|
    Color.rgb 1. 0. 0.; Color.rgb 0. 1. 0.; Color.rgb 0. 0. 1.; Color.rgb 1. 1. 0.;
  |]

let init () : int * regl_output list =
  ( 0,
    [
      start_regl
        {
          virt_width = 1280.;
          virt_height = 800.;
          fbo_num = 2;
          builtin_programs = None;
          window = { default_window_config with resizable = Some false };
          app_name = None;
        };
    ] )

let update (frame : int) (input : regl_input) :
    int * Regl_audio.audio * regl_output list =
  match input with
  | Regl_proto.Event (Regl_proto.UpdateTick _) ->
      (frame + 1, Regl_audio.silence, [])
  | _ -> (frame, Regl_audio.silence, [])

let view (frame : int) : Regl_common.renderable =
  Regl_builtin_programs.clear colors.(frame mod Array.length colors)

let () = Regl_backend.create_app init update view
