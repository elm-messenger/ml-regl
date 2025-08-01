open Js_of_ocaml

type program_call = (string * Js.Unsafe.any) list

type regl_effect = program_call

type camera = {
  x : float;
  y : float;
  zoom : float;
  rotation : float;
}

type renderable = 
  | AtomicRenderable of program_call
  | GroupRenderable of regl_effect list * renderable list  
  | GroupRenderableWithCamera of camera * regl_effect list * renderable list

val render : renderable -> Js.Unsafe.any

val group : regl_effect list -> renderable list -> renderable

val group_with_camera : camera -> regl_effect list -> renderable list -> renderable

val update_field : string -> Js.Unsafe.any -> renderable -> renderable

val get_field : string -> renderable -> Js.Unsafe.any option

val gen_prog : program_call -> renderable

val to_rgba_list : Color.t -> float list