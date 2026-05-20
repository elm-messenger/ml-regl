(** Unified, platform-agnostic ml_regl entry point.

    This is the public interface of the virtual library [regl_backend].
    Two implementations exist, selected at link time:

    - [ml_regl_js]      → uses [Js.export "MlApp"] + the browser host.
    - [ml_regl_desktop] → uses [Callback.register] + the libdeclgl host.

    Both produce identical observable behaviour from the user's [init],
    [update], and [view] functions: the platform difference (event source,
    rendering surface, audio backend, lifetime) is fully hidden. *)

open Ml_regl_core
open Ml_regl_core.Regl_proto

val create_app :
  (unit -> 'a * regl_output list) ->
  ('a -> regl_input -> 'a * Regl_audio.audio * regl_output list) ->
  ('a -> Regl_common.renderable) ->
  unit
(** Spin up the application and start the platform's event/render loop.
    The exact lifecycle differs per backend:

    - JS:      registers the [MlApp] global; the browser host drives
               [requestAnimationFrame]. [create_app] returns immediately.
    - Desktop: opens the SDL3 window in response to the [StartRegl]
               command emitted by [init], then enters the render loop
               on the OCaml main thread. [create_app] returns when the
               user closes the window. *)
