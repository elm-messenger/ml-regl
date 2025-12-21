open Ml_regl
open Js_of_ocaml

let mycircle = Regl_builtin_programs.circle (400., 300.) 100. Color.red

let mytext txt =
  Regl_builtin_programs.textbox_centered (400., 300.) 50. txt "consolas"
    Color.black

(* Start the app *)

type model = { start_time : float; last_time : float }

let shapes (m : model) =
  let numx = 50. in
  let numy = 30. in
  let nx = int_of_float numx * 2 in
  let ny = int_of_float numy * 2 in
  let t = m.last_time *. 30. in
  let acc = ref [] in

  for x = 0 to nx do
    let fx = float_of_int x /. numx *. 1920. in
    for y = 0 to ny do
      let fy = float_of_int y /. numy *. 1000. in
      acc :=
        Regl_builtin_programs.triangle
          (t +. fx, fy +. 15.)
          (t +. fx +. 15., fy +. 45.)
          (t +. fx +. 30., fy +. 15.)
          Color.red
        :: !acc
    done
  done;
  (* List.rev !acc *)
  !acc

let view (m : model) =
  Regl_common.group [] ([ Regl_builtin_programs.clear Color.white ] @ shapes m)

let init (canvas : Dom_html.canvasElement Js.t option) _ =
  let startconfig : Regl.regl_start_config =
    {
      virt_width = 1920.;
      virt_height = 1080.;
      fbo_num = 5;
      builtin_programs = None;
    }
  in
  let mc = Option.get canvas in
  mc##.width := 1280;
  mc##.height := 720;
  Regl.execCmd (Regl.start_regl startconfig);
  { start_time = 0.0; last_time = 0.0 }

let update (canvas : Dom_html.canvasElement Js.t option) (m : model)
    (e : Regl.regl_input) =
  let nm =
    match e with
    | Regl.Tick ts ->
        {
          last_time =
            (if m.start_time = 0.0 then 0. else (ts -. m.start_time) /. 1000.);
          start_time = (if m.start_time = 0.0 then ts else m.start_time);
        }
    | Regl.Event event ->
        (* Js.Unsafe.global##.console##log (Js.to_string event##._type); *)
        m
    | Regl.REGLRecvMsg _ -> m
  in
  (nm, view nm, [])

let _ = Regl.create_app init update
