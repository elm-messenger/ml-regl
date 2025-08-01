open Js_of_ocaml
open Regl_common

let dst_over_src src dst =
  gen_prog [
    ("_c", Js.Unsafe.inject (Js.number_of_float 3.0));
    ("_p", Js.Unsafe.inject (Js.string "defaultCompositor"));
    ("r1", render src);
    ("r2", render dst);
    ("mode", Js.Unsafe.inject (Js.number_of_float 0.0))
  ]

let mask_by_src src dst =
  gen_prog [
    ("_c", Js.Unsafe.inject (Js.number_of_float 3.0));
    ("_p", Js.Unsafe.inject (Js.string "defaultCompositor"));
    ("r1", render src);
    ("r2", render dst);
    ("mode", Js.Unsafe.inject (Js.number_of_float 1.0))
  ]

let img_fade mask t invert src dst =
  gen_prog [
    ("_c", Js.Unsafe.inject (Js.number_of_float 3.0));
    ("_p", Js.Unsafe.inject (Js.string "imgFade"));
    ("r1", render src);
    ("r2", render dst);
    ("mask", Js.Unsafe.inject (Js.string mask));
    ("t", Js.Unsafe.inject (Js.number_of_float t));
    ("invert_mask", Js.Unsafe.inject (Js.number_of_float (if invert then 1.0 else 0.0)))
  ]

let linear_fade t src dst =
  gen_prog [
    ("_c", Js.Unsafe.inject (Js.number_of_float 3.0));
    ("_p", Js.Unsafe.inject (Js.string "compFade"));
    ("r1", render src);
    ("r2", render dst);
    ("mode", Js.Unsafe.inject (Js.number_of_float 0.0));
    ("t", Js.Unsafe.inject (Js.number_of_float t))
  ]