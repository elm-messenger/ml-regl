type camera = { x : float; y : float; zoom : float; rotation : float }

module Render_pb = Transport_render.Mlregl.Transport.Render
module Common_pb = Transport_common.Mlregl.Transport.Common

type program_call = Render_pb.ProgramCallField.t list
type regl_effect = Render_pb.Effect.t
type renderable = Render_pb.Renderable.t

let field key v = Render_pb.ProgramCallField.make ~key ~val':v ()
let num key v = field key (Common_pb.Value.make ~kind:(`Number_value v) ())
let str key v = field key (Common_pb.Value.make ~kind:(`String_value v) ())
let bool key v = field key (Common_pb.Value.make ~kind:(`Bool_value v) ())

let nums key values =
  let number_array_value = Common_pb.ScalarArray.make ~values () in
  field key
    (Common_pb.Value.make ~kind:(`Number_array_value number_array_value) ())

let strs key values =
  let string_array_value = Common_pb.StringArray.make ~values () in
  field key
    (Common_pb.Value.make ~kind:(`String_array_value string_array_value) ())

let atomic (program : string) (pc : program_call) : renderable =
  Render_pb.Renderable.make
    ~kind:(`Atomic (Render_pb.AtomicRenderable.make ~fields:pc ~program ()))
    ()

let mk_effect (program : string) (pc : program_call) : regl_effect =
  Render_pb.Effect.make ~fields:pc ~program ()

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
    ~kind:
      (`Group (Render_pb.GroupRenderable.make ~effects ~children ~camera ()))
    ()

let composite (program : string) (pc : program_call) (left : renderable)
    (right : renderable) : renderable =
  let compositor = Render_pb.AtomicRenderable.make ~fields:pc ~program () in
  Render_pb.Renderable.make
    ~kind:
      (`Composite
         (Render_pb.CompositeRenderable.make ~compositor ~left ~right ()))
    ()

let encode_frame_pb (r : renderable) : bytes =
  Render_pb.Renderable.to_proto r
  |> Ocaml_protoc_plugin.Writer.contents |> Bytes.unsafe_of_string

let to_rgba_list color =
  let rgba = Color.to_rgba color in
  [ rgba.red; rgba.green; rgba.blue; rgba.alpha ]
