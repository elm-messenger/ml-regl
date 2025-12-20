open Ml_regl
open Js_of_ocaml

let mycircle = Regl_builtin_programs.circle (3., 4.) 5. Color.red

(* Start the app *)

type model = { num : float }

let init _ = { num = 0.0 }

let update (m : model) (e : Regl.regl_input) =
  let nm =
    match e with
    | Regl.Tick ts -> { num = ts }
    | Regl.Event _ -> m
    | Regl.REGLRecvMsg _ -> m
  in
  (nm, mycircle, [])

let canvas = Regl.create_app init update
