open Js_of_ocaml
open Regl_common

type primitive =
  | Points
  | Lines
  | LineLoop
  | LineStrip
  | Triangles
  | TriangleStrip
  | TriangleFan

val primitive_to_value : primitive -> Js.Unsafe.any

val empty : renderable
val clear : Color.t -> renderable
val triangle : (float * float) -> (float * float) -> (float * float) -> Color.t -> renderable
val quad : (float * float) -> (float * float) -> (float * float) -> (float * float) -> Color.t -> renderable
val rect_centered : (float * float) -> (float * float) -> float -> Color.t -> renderable
val rect : (float * float) -> (float * float) -> Color.t -> renderable
val poly : (float * float) list -> Color.t -> renderable
val lines : ((float * float) * (float * float)) list -> Color.t -> renderable
val linestrip : (float * float) list -> Color.t -> renderable
val lineloop : (float * float) list -> Color.t -> renderable
val function_curve : (float -> float) -> (float * float) -> (float * float) -> float -> Color.t -> renderable
val poly_prim : (float * float) list -> float list -> Color.t -> primitive -> renderable
val circle : (float * float) -> float -> Color.t -> renderable
val rounded_rect : (float * float) -> (float * float) -> float -> Color.t -> renderable
val texture : (float * float) -> (float * float) -> (float * float) -> (float * float) -> string -> renderable
val texture_cropped : (float * float) -> (float * float) -> (float * float) -> (float * float) -> (float * float) -> (float * float) -> (float * float) -> (float * float) -> string -> renderable
val rect_texture : (float * float) -> (float * float) -> string -> renderable
val rect_texture_cropped : (float * float) -> (float * float) -> (float * float) -> (float * float) -> string -> renderable
val centered_texture : (float * float) -> (float * float) -> float -> string -> renderable
val centered_texture_cropped : (float * float) -> (float * float) -> float -> (float * float) -> (float * float) -> string -> renderable

val texture_with_alpha : (float * float) -> (float * float) -> (float * float) -> (float * float) -> float -> string -> renderable
val texture_cropped_with_alpha : (float * float) -> (float * float) -> (float * float) -> (float * float) -> (float * float) -> (float * float) -> (float * float) -> (float * float) -> float -> string -> renderable
val rect_texture_with_alpha : (float * float) -> (float * float) -> float -> string -> renderable
val rect_texture_cropped_with_alpha : (float * float) -> (float * float) -> (float * float) -> (float * float) -> float -> string -> renderable
val centered_texture_with_alpha : (float * float) -> (float * float) -> float -> float -> string -> renderable
val centered_texture_cropped_with_alpha : (float * float) -> (float * float) -> float -> (float * float) -> (float * float) -> float -> string -> renderable

type textbox_option = {
  fonts : string list;
  text : string;
  size : float;
  color : Color.t;
  word_break : bool;
  thickness : float option;
  italic : float option;
  width : float option;
  line_height : float option;
  word_spacing : float option;
  align : string option;
  tab_size : float option;
  valign : string option;
  letter_spacing : float option;
}

val default_textbox_option : textbox_option
val textbox : (float * float) -> float -> string -> string -> Color.t -> renderable
val textbox_mf : (float * float) -> float -> string -> string list -> Color.t -> renderable
val textbox_centered : (float * float) -> float -> string -> string -> Color.t -> renderable
val textbox_mf_centered : (float * float) -> float -> string -> string list -> Color.t -> renderable
val textbox_pro : (float * float) -> textbox_option -> renderable
val save_as_texture : string -> renderable