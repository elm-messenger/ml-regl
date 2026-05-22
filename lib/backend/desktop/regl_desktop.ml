(* Desktop facade for ml_regl. See regl_desktop.mli. *)

open Ml_regl_core
open Ml_regl_core.Regl_proto

(* The two primitives implemented by libdeclgl (declgl-desktop/src/caml_bridge).
   Bytes-typed parameters are zero-copied via Bytes_val on the C side. There is
   no separate "run loop" external — libdeclgl reacts to [StartRegl] inside
   [declgl_ship_backend_cmd], opens the window, and enters its loop right there.
   The call returns when the user closes the window. *)
external declgl_ship_backend_cmd : bytes -> unit = "declgl_ship_backend_cmd"
external declgl_ship_audio_cmd : bytes -> unit = "declgl_ship_audio_cmd"

let execCmdPb commands =
  declgl_ship_backend_cmd (encode_backend_command_batch_pb commands)

let execAudioCmdPb actions =
  declgl_ship_audio_cmd (Regl_audio.encode_command_batch_pb actions)

(* The desktop host: byte-shippers go straight into the engine via the external
   primitives above. There is no additional buffering. *)
module DesktopHost = struct
  let ship_backend_cmd payload = declgl_ship_backend_cmd payload
  let ship_audio_cmd payload = declgl_ship_audio_cmd payload
end

module DesktopRuntime = Regl_runtime.Make (DesktopHost)

let create_app (init : unit -> 'a * regl_output list)
    (update : 'a -> regl_input -> 'a * Regl_audio.audio * regl_output list)
    (view : 'a -> Regl_common.renderable) =
  let h = DesktopRuntime.create_app ~init ~update ~view in

  (* C++ side resolves these by name with caml_named_value. The names mirror the
     [MlApp] keys that the JS backend sees via [Js.export "MlApp"]. *)
  Callback.register "declgl_app_update" h.update;
  Callback.register "declgl_app_event" h.event;
  Callback.register "declgl_app_view" h.view;
  Callback.register "declgl_app_recv_regl_cmd_pb" h.recv_regl_cmd_pb;
  Callback.register "declgl_app_recv_audio_msg_pb" h.recv_audio_msg_pb;

  (* Runs user-side init, ships the resulting BackendCommandBatch via
     [declgl_ship_backend_cmd], which — upon seeing [StartRegl] — opens the
     window and enters the C++ run loop. Returns only when the user closes the
     window. *)
  h.init ()
