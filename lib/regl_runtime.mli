(** Host-agnostic runtime driver for ml_regl applications.

    This module contains the [create_app] state machine that used to live in
    [Regl.create_app] (JS-only). It's now parameterized over a [Host] module so
    the same logic can be reused by:
    - the JS facade ([Regl.create_app]), with a host that talks to
      [Js.Unsafe.global##.MlREGL] and [Dom_html.canvasElement Js.t];
    - the native [declgl-desktop] capture tool, with a host that writes to a
      file;
    - eventually the native runtime itself, with a host that calls into the
      [libdeclgl] C ABI.

    No [Js_of_ocaml] dependency. *)

(** What a host has to provide to drive an ml_regl app:
    - byte-shipping primitives for backend and audio command batches. *)
module type Host = sig
  val ship_backend_cmd : bytes -> unit
  (** Send an encoded [BackendCommandBatch] to the backend. Called once per
      non-empty cmd list produced by [init] / [update]. *)

  val ship_audio_cmd : bytes -> unit
  (** Send an encoded [AudioCommandBatch] to the backend. Called once per
      non-empty action list produced by audio diffing. *)
end

(** Build a runtime over a host. The result is a record of closures that the
    host's own event/animation loop calls. *)
module Make (H : Host) : sig
  type 'model handle = {
    init : unit -> unit;
    update : float -> unit;
    event : bytes -> unit;
    view : unit -> bytes option;
    recv_regl_cmd_pb : bytes -> unit;
    recv_audio_msg_pb : bytes -> unit;
  }

  val create_app :
    init:(unit -> 'model * Regl_proto.regl_output list) ->
    update:
      ('model ->
      Regl_proto.regl_input ->
      'model * Regl_audio.audio * Regl_proto.regl_output list) ->
    view:('model -> Regl_common.renderable) ->
    'model handle
end
