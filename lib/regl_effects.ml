open Regl_common

let rec blur radius = [ blur_h radius; blur_v radius ]
and blur_h radius = mk_effect "blurh" [ num "radius" radius ]
and blur_v radius = mk_effect "blurv" [ num "radius" radius ]

let alpha_mult a = mk_effect "alphamult" [ num "alpha" a ]
let color_mult r g b a = mk_effect "colormult" [ nums "color" [ r; g; b; a ] ]
let pixilation ps = mk_effect "pixilation" [ num "ps" ps ]

let outline o color =
  let rgba_list = to_rgba_list color in
  mk_effect "outline" [ num "outline" o; nums "color" rgba_list ]

let fxaa = mk_effect "fxaa" []

let rec gblur radius =
  [
    gblur_h (radius *. 8.0);
    gblur_v (radius *. 7.0);
    gblur_h (radius *. 6.0);
    gblur_v (radius *. 5.0);
    gblur_h (radius *. 4.0);
    gblur_v (radius *. 3.0);
    gblur_h (radius *. 2.0);
    gblur_v (radius *. 1.0);
  ]

and gblur_h r = mk_effect "gblurh" [ num "radius" r ]
and gblur_v r = mk_effect "gblurv" [ num "radius" r ]

let crt count = mk_effect "crt" [ num "count" count ]
