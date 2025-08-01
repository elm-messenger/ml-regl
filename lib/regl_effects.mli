open Regl_common

val blur : float -> regl_effect list
val blur_h : float -> regl_effect
val blur_v : float -> regl_effect
val alpha_mult : float -> regl_effect
val color_mult : float -> float -> float -> float -> regl_effect
val pixilation : float -> regl_effect
val outline : float -> Color.t -> regl_effect
val fxaa : regl_effect
val gblur : float -> regl_effect list
val gblur_h : float -> regl_effect
val gblur_v : float -> regl_effect
val crt : float -> regl_effect
