open Js_of_ocaml

type camera = { x : float; y : float; zoom : float; rotation : float }
module Render_pb = Transport_render.Mlregl.Transport.Render

type program_call = Render_pb.ProgramCallField.t list
type regl_effect = Render_pb.Effect.t
type renderable = Render_pb.Renderable.t

let num key v =
  Render_pb.ProgramCallField.make ~key ~value:(`Number_value v) ()

let str key v =
  Render_pb.ProgramCallField.make ~key ~value:(`String_value v) ()

let bool key v =
  Render_pb.ProgramCallField.make ~key ~value:(`Bool_value v) ()

let nums key values =
  let number_array_value = Render_pb.ScalarArray.make ~values () in
  Render_pb.ProgramCallField.make ~key ~value:(`Number_array_value number_array_value)
    ()

let strs key values =
  let string_array_value = Render_pb.StringArray.make ~values () in
  Render_pb.ProgramCallField.make ~key ~value:(`String_array_value string_array_value)
    ()

let bytes key v =
  Render_pb.ProgramCallField.make ~key ~value:(`Binary_value v) ()

let renderable_to_bytes (r : renderable) : bytes =
  Render_pb.Renderable.to_proto r |> Ocaml_protoc_plugin.Writer.contents
  |> Bytes.unsafe_of_string

let renderable_bytes key r = bytes key (renderable_to_bytes r)

let atomic (pc : program_call) : renderable =
  Render_pb.Renderable.make
    ~kind:(`Atomic (Render_pb.AtomicRenderable.make ~fields:pc ()))
    ()

let mk_effect (pc : program_call) : regl_effect =
  Render_pb.Effect.make ~fields:pc ()

let group effects children =
  Render_pb.Renderable.make
    ~kind:(`Group (Render_pb.GroupRenderable.make ~effects ~children ()))
    ()

let group_with_camera camera effects children =
  let camera =
    Render_pb.Camera.make ~x:camera.x ~y:camera.y ~zoom:camera.zoom
      ~rotation:camera.rotation ()
  in
  Render_pb.Renderable.make
    ~kind:(`Group (Render_pb.GroupRenderable.make ~effects ~children ~camera ()))
    ()

let encode_frame_pb (r : renderable) : bytes =
  Render_pb.RenderFrame.make ~root:r ()
  |> Render_pb.RenderFrame.to_proto
  |> Ocaml_protoc_plugin.Writer.contents
  |> Bytes.unsafe_of_string

let to_rgba_list color =
  let rgba = Color.to_rgba color in
  [ rgba.red; rgba.green; rgba.blue; rgba.alpha ]
