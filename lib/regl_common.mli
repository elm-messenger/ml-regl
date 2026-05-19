open Js_of_ocaml

type camera = { x : float; y : float; zoom : float; rotation : float }
module Render_pb = Transport_render.Mlregl.Transport.Render

type program_call = Render_pb.ProgramCallField.t list
type regl_effect = Render_pb.Effect.t
type renderable = Render_pb.Renderable.t

val num : string -> float -> Render_pb.ProgramCallField.t
val str : string -> string -> Render_pb.ProgramCallField.t
val bool : string -> bool -> Render_pb.ProgramCallField.t
val nums : string -> float list -> Render_pb.ProgramCallField.t
val strs : string -> string list -> Render_pb.ProgramCallField.t
val renderable : string -> renderable -> Render_pb.ProgramCallField.t

val encode_frame_pb : renderable -> bytes
val atomic : program_call -> renderable
val mk_effect : program_call -> regl_effect
val group : regl_effect list -> renderable list -> renderable
val group_with_camera :
  camera -> regl_effect list -> renderable list -> renderable
val composite : program_call -> renderable -> renderable -> renderable
val to_rgba_list : Color.t -> float list
