open Ml_regl
open Js_of_ocaml

let mycircle = Regl_builtin_programs.circle (400., 300.) 100. Color.red

(* Start the app *)

type model = { num : float }

let init (canvas : Dom_html.canvasElement Js.t option) _ =
  let startconfig : Regl.regl_start_config =
    {
      virt_width = 800.;
      virt_height = 600.;
      fbo_num = 5;
      builtin_programs = None;
    }
  in
  let mc = Option.get canvas in
  mc##.width := 800;
  mc##.height := 600;
  Regl.execCmd (Regl.start_regl startconfig);
  { num = 0.0 }

let update (canvas : Dom_html.canvasElement Js.t option) (m : model)
    (e : Regl.regl_input) =
  let nm =
    match e with
    | Regl.Tick ts -> { num = ts }
    | Regl.Event _ -> m
    | Regl.REGLRecvMsg _ -> m
  in
  (nm, mycircle, [])

let _ = Regl.create_app init update
