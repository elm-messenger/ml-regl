open Js_of_ocaml
open Regl_common

let rec blur radius = [blur_h radius; blur_v radius]

and blur_h radius = [
  ("_p", Js.Unsafe.inject (Js.string "blurh"));
  ("radius", Js.Unsafe.inject (Js.number_of_float radius))
]

and blur_v radius = [
  ("_p", Js.Unsafe.inject (Js.string "blurv"));
  ("radius", Js.Unsafe.inject (Js.number_of_float radius))
]

let alpha_mult a = [
  ("_p", Js.Unsafe.inject (Js.string "alphamult"));
  ("alpha", Js.Unsafe.inject (Js.number_of_float a))
]

let color_mult r g b a =
  let color_array = Js.array (Array.of_list (List.map Js.number_of_float [r; g; b; a])) in
  [
    ("_p", Js.Unsafe.inject (Js.string "colormult"));
    ("color", Js.Unsafe.inject color_array)
  ]

let pixilation ps = [
  ("_p", Js.Unsafe.inject (Js.string "pixilation"));
  ("ps", Js.Unsafe.inject (Js.number_of_float ps))
]

let outline o color =
  let rgba_list = to_rgba_list color in
  let color_array = Js.array (Array.of_list (List.map Js.number_of_float rgba_list)) in
  [
    ("_p", Js.Unsafe.inject (Js.string "outline"));
    ("outline", Js.Unsafe.inject (Js.number_of_float o));
    ("color", Js.Unsafe.inject color_array)
  ]

let fxaa = [
  ("_p", Js.Unsafe.inject (Js.string "fxaa"))
]

let rec gblur radius = [
  gblur_h (radius *. 8.0);
  gblur_v (radius *. 7.0);
  gblur_h (radius *. 6.0);
  gblur_v (radius *. 5.0);
  gblur_h (radius *. 4.0);
  gblur_v (radius *. 3.0);
  gblur_h (radius *. 2.0);
  gblur_v (radius *. 1.0);
]

and gblur_h r = [
  ("_p", Js.Unsafe.inject (Js.string "gblurh"));
  ("radius", Js.Unsafe.inject (Js.number_of_float r))
]

and gblur_v r = [
  ("_p", Js.Unsafe.inject (Js.string "gblurv"));
  ("radius", Js.Unsafe.inject (Js.number_of_float r))
]

let crt count = [
  ("_p", Js.Unsafe.inject (Js.string "crt"));
  ("count", Js.Unsafe.inject (Js.number_of_float count))
]