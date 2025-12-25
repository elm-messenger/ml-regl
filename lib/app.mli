open Js_of_ocaml

val create_app :
  (Dom_html.canvasElement Js.t option -> Js.Unsafe.any -> 'a) ->
  'a Regl.user_update_t ->
  unit
