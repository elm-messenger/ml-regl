open Regl_common

let dst_over_src src dst =
  atomic
    [
      num "_c" 3.0;
      str "_p" "defaultCompositor";
      num "mode" 0.0;
      renderable_bytes "r1" src;
      renderable_bytes "r2" dst;
    ]

let mask_by_src src dst =
  atomic
    [
      num "_c" 3.0;
      str "_p" "defaultCompositor";
      num "mode" 1.0;
      renderable_bytes "r1" src;
      renderable_bytes "r2" dst;
    ]

let img_fade mask t invert src dst =
  atomic
    [
      num "_c" 3.0;
      str "_p" "imgFade";
      str "mask" mask;
      num "t" t;
      num "invert_mask" (if invert then 1.0 else 0.0);
      renderable_bytes "r1" src;
      renderable_bytes "r2" dst;
    ]

let linear_fade t src dst =
  atomic
    [
      num "_c" 3.0;
      str "_p" "compFade";
      num "mode" 0.0;
      num "t" t;
      renderable_bytes "r1" src;
      renderable_bytes "r2" dst;
    ]
