(** Browser-targeted [ml_regl] facade. Re-exports the portable core
    ([Regl_proto]) and adds the JS-only host glue: the [Js.export "MlApp"]
    bridge and DOM-event-typed [regl_event]. *)

(* include module type of Regl_proto *)
open Ml_regl_core
open Ml_regl_core.Regl_proto
open Ml_regl_core.Regl_runtime

val execCmdPb : Backend_pb.BackendCommand.t list -> unit
(** Encode + ship a [BackendCommandBatch] via the global [MlREGL] host. *)

val execAudioCmdPb : Regl_audio.audio_action list -> unit
(** Encode + ship an [AudioCommandBatch] via the global [MlREGL] host. *)

val create_app :
  (unit -> 'a * regl_output list) ->
  ('a -> regl_input -> 'a * Regl_audio.audio * regl_output list) ->
  ('a -> Regl_common.renderable) ->
  unit
(** Spin up [MlApp] (init, update, view, recvREGLCmdPb, recvAudioMsgPb). *)
