open Ml_regl
open Js_of_ocaml

let mycircle = Regl_builtin_programs.circle (3., 4.) 5. Color.red
let canvas = Regl.init_regl ()

let _ =
  Js.export "App"
    (Js.Unsafe.obj
       [|
         ( "canvas",
           Js.Unsafe.inject (fun _ ->
               match !canvas with
               | Some c -> c
               | None -> failwith "No canvas initialized") );
       |])
