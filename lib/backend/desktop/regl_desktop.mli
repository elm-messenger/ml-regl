(** Desktop-targeted [ml_regl] facade. Mirrors [Regl_js] but talks to a
    native C++ runtime ([libdeclgl] in the [declgl-desktop] sibling project)
    instead of a browser host.

    Architecture: OCaml owns the process and main thread. The user
    program's top-level finishes with
    [Regl_desktop.create_app init update view], which:

    1. instantiates [Regl_runtime.Make(DesktopHost)] and builds the runtime
       [handle] of closures;
    2. registers each closure under a well-known name via [Callback.register]
       so the C++ side can resolve it with [caml_named_value];
    3. calls [h.init ()]. The init closure runs the user-provided init
       function and ships its output commands. When libdeclgl sees the
       [StartRegl] command in that batch, it opens the SDL3 window, creates
       a GL 3.3 Core context, and enters the per-frame loop *from inside*
       [declgl_ship_backend_cmd] — exactly mirroring the JS backend, where
       [start()] is what triggers [requestAnimationFrame(step)].

    The loop calls back into OCaml on every event ([h.event]) / tick
    ([h.update]) / frame ([h.view]) using [caml_callback], all on the same
    thread (so no [caml_acquire_runtime_system] dance is needed). When the
    user closes the window, the loop returns, [declgl_ship_backend_cmd]
    returns, [h.init] returns, and [create_app] returns to the user. *)

open Ml_regl_core
open Ml_regl_core.Regl_proto

val execCmdPb : Backend_pb.BackendCommand.t list -> unit
(** Encode + ship a [BackendCommandBatch] via the libdeclgl host. *)

val execAudioCmdPb : Regl_audio.audio_action list -> unit
(** Encode + ship an [AudioCommandBatch] via the libdeclgl host. *)

val create_app :
  (unit -> 'a * regl_output list) ->
  ('a -> regl_input -> 'a * Regl_audio.audio * regl_output list) ->
  ('a -> Regl_common.renderable) ->
  unit
(** Register the runtime closures with libdeclgl, then call the runtime's
    [init]. The first command batch produced by [init] should contain a
    [StartRegl] command — that's what makes libdeclgl open the window and
    enter its main loop. Blocks until the user closes the window. *)
