open Regl_common

val dst_over_src : renderable -> renderable -> renderable
val mask_by_src : renderable -> renderable -> renderable
val img_fade : string -> float -> bool -> renderable -> renderable -> renderable
val linear_fade : float -> renderable -> renderable -> renderable