open Regl_common

let dst_over_src src dst =
  composite [ str "_p" "defaultCompositor"; num "mode" 0.0 ] src dst

let mask_by_src src dst =
  composite [ str "_p" "defaultCompositor"; num "mode" 1.0 ] src dst

let img_fade mask t invert src dst =
  composite
    [
      str "_p" "imgFade";
      str "mask" mask;
      num "t" t;
      num "invert_mask" (if invert then 1.0 else 0.0);
    ]
    src dst

let linear_fade t src dst =
  composite [ str "_p" "compFade"; num "mode" 0.0; num "t" t ] src dst
