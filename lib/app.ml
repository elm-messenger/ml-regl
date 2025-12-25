open Js_of_ocaml

(* Creating the canvas app. Exposing MlApp. *)
let create_app
    (init : Dom_html.canvasElement Js.t option -> Js.Unsafe.any -> 'a)
    (update : 'a Regl.user_update_t) =
  let canvas : Dom_html.canvasElement Js.t option ref = ref None in
  let model : 'a option ref = ref None in
  let update_regl_model input = Regl.update_model input model update !canvas in
  Js.export "MlApp"
    (Js.Unsafe.obj
       [|
         ("bind", Js.Unsafe.inject (fun c -> canvas := Some c));
         ("init", Js.Unsafe.inject (fun c -> model := Some (init !canvas c)));
         ("update", Js.Unsafe.inject (fun ts -> update_regl_model (Tick ts)));
         ("event", Js.Unsafe.inject (fun ev -> update_regl_model (Event ev)));
         ( "recvREGLCmd",
           Js.Unsafe.inject (fun recvcmd ->
               match Regl.decode_recv_msg recvcmd with
               | Some msg -> update_regl_model (REGLRecvMsg msg)
               | None -> Js.Unsafe.inject Js.null) );
       |])
