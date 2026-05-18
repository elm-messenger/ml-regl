open Regl_common

type primitive =
  | Points
  | Lines
  | LineLoop
  | LineStrip
  | Triangles
  | TriangleStrip
  | TriangleFan

let primitive_to_string = function
  | Points -> "points"
  | Lines -> "lines"
  | LineLoop -> "line loop"
  | LineStrip -> "line strip"
  | Triangles -> "triangles"
  | TriangleStrip -> "triangle strip"
  | TriangleFan -> "triangle fan"

let empty = atomic []

let clear color =
  let rgba_list = to_rgba_list color in
  atomic
    [
      num "_c" 1.0;
      str "_n" "clear";
      nums "color" rgba_list;
      num "depth" 1.0;
    ]

let triangle (x1, y1) (x2, y2) (x3, y3) color =
  let rgba_list = to_rgba_list color in
  atomic
    [
      num "_c" 0.0;
      str "_p" "triangle";
      nums "pos" [ x1; y1; x2; y2; x3; y3 ];
      nums "color" rgba_list;
    ]

let quad (x1, y1) (x2, y2) (x3, y3) (x4, y4) color =
  let rgba_list = to_rgba_list color in
  atomic
    [
      num "_c" 0.0;
      str "_p" "quad";
      nums "pos" [ x1; y1; x2; y2; x3; y3; x4; y4 ];
      nums "color" rgba_list;
    ]

let rect_centered (x, y) (w, h) angle color =
  let rgba_list = to_rgba_list color in
  atomic
    [
      num "_c" 0.0;
      str "_p" "rect";
      nums "posize" [ x; y; w; h ];
      num "angle" angle;
      nums "color" rgba_list;
    ]

let rect (x, y) (w, h) color =
  rect_centered (x +. (w /. 2.0), y +. (h /. 2.0)) (w, h) 0.0 color

let poly xs color =
  let pos = List.concat_map (fun (x, y) -> [ x; y ]) xs in
  let elem =
    List.concat_map
      (fun x -> [ 0.0; float_of_int x; float_of_int x +. 1.0 ])
      (List.init (List.length xs - 2) (fun i -> i + 1))
  in
  let rgba_list = to_rgba_list color in
  atomic
    [
      num "_c" 0.0;
      str "_p" "poly";
      nums "pos" pos;
      nums "elem" elem;
      nums "color" rgba_list;
    ]

let lines xs color =
  let pos =
    List.concat_map (fun ((x1, y1), (x2, y2)) -> [ x1; y1; x2; y2 ]) xs
  in
  let elem = List.init (2 * List.length xs) float_of_int in
  let rgba_list = to_rgba_list color in
  atomic
    [
      num "_c" 0.0;
      str "_p" "poly";
      nums "pos" pos;
      nums "elem" elem;
      nums "color" rgba_list;
      str "prim" (primitive_to_string Lines);
    ]

let linestrip xs color =
  let pos = List.concat_map (fun (x, y) -> [ x; y ]) xs in
  let elem = List.init (List.length xs) float_of_int in
  let rgba_list = to_rgba_list color in
  atomic
    [
      num "_c" 0.0;
      str "_p" "poly";
      nums "pos" pos;
      nums "elem" elem;
      nums "color" rgba_list;
      str "prim" (primitive_to_string LineStrip);
    ]

let lineloop xs color =
  let pos = List.concat_map (fun (x, y) -> [ x; y ]) xs in
  let elem = List.init (List.length xs) float_of_int in
  let rgba_list = to_rgba_list color in
  atomic
    [
      num "_c" 0.0;
      str "_p" "poly";
      nums "pos" pos;
      nums "elem" elem;
      nums "color" rgba_list;
      str "prim" (primitive_to_string LineLoop);
    ]

let function_curve f (x, y) (left, right) freq color =
  let samples = int_of_float (ceil (freq *. (right -. left))) in
  let xs =
    List.init (samples + 1) (fun u ->
        (float_of_int u /. float_of_int samples *. (right -. left)) +. left)
  in
  let xys = List.map (fun posx -> (posx +. x, f posx +. y)) xs in
  linestrip xys color

let poly_prim xs elem color prim =
  let pos = List.concat_map (fun (x, y) -> [ x; y ]) xs in
  let rgba_list = to_rgba_list color in
  atomic
    [
      num "_c" 0.0;
      str "_p" "poly";
      nums "pos" pos;
      nums "elem" elem;
      nums "color" rgba_list;
      str "prim" (primitive_to_string prim);
    ]

let circle (x1, y1) r color =
  let rgba_list = to_rgba_list color in
  atomic
    [
      num "_c" 0.0;
      str "_p" "circle";
      nums "cr" [ x1; y1; r ];
      nums "color" rgba_list;
    ]

let rounded_rect (x1, y1) (w, h) r color =
  let rgba_list = to_rgba_list color in
  atomic
    [
      num "_c" 0.0;
      str "_p" "roundedRect";
      nums "cs" [ x1; y1; w; h ];
      num "radius" r;
      nums "color" rgba_list;
    ]

let texture (x1, y1) (x2, y2) (x3, y3) (x4, y4) name =
  atomic
    [
      num "_c" 0.0;
      str "_p" "texture";
      str "texture" name;
      nums "pos" [ x1; y1; x2; y2; x3; y3; x4; y4 ];
    ]

let texture_cropped (x1, y1) (x2, y2) (x3, y3) (x4, y4) (cx1, cy1) (cx2, cy2)
    (cx3, cy3) (cx4, cy4) name =
  atomic
    [
      num "_c" 0.0;
      str "_p" "textureCropped";
      str "texture" name;
      nums "pos" [ x1; y1; x2; y2; x3; y3; x4; y4 ];
      nums "texc" [ cx1; cy1; cx2; cy2; cx3; cy3; cx4; cy4 ];
    ]

let rec rect_texture (x, y) (w, h) name =
  centered_texture (x +. (w /. 2.0), y +. (h /. 2.0)) (w, h) 0.0 name

and centered_texture (x, y) (w, h) angle name =
  atomic
    [
      num "_c" 0.0;
      str "_p" "centeredTexture";
      str "texture" name;
      nums "posize" [ x; y; w; h ];
      num "angle" angle;
    ]

let rect_texture_cropped (x, y) (w, h) (cx, cy) (cw, ch) name =
  atomic
    [
      num "_c" 0.0;
      str "_p" "textureCropped";
      str "texture" name;
      nums "pos" [ x; y; x +. w; y; x +. w; y +. h; x; y +. h ];
      nums "texc"
        [
          cx;
          1.0 -. cy;
          cx +. cw;
          1.0 -. cy;
          cx +. cw;
          1.0 -. cy -. ch;
          cx;
          1.0 -. cy -. ch;
        ];
    ]

let centered_texture_cropped (x, y) (w, h) angle (cx, cy) (cw, ch) name =
  atomic
    [
      num "_c" 0.0;
      str "_p" "centeredCroppedTexture";
      str "texture" name;
      nums "posize" [ x; y; w; h ];
      num "angle" angle;
      nums "texc" [ cx; cy; cw; ch ];
    ]

(* Functions with alpha *)
let texture_with_alpha (x1, y1) (x2, y2) (x3, y3) (x4, y4) alpha name =
  atomic
    [
      num "_c" 0.0;
      str "_p" "texture";
      str "texture" name;
      nums "pos" [ x1; y1; x2; y2; x3; y3; x4; y4 ];
      num "alpha" alpha;
    ]

let texture_cropped_with_alpha (x1, y1) (x2, y2) (x3, y3) (x4, y4) (cx1, cy1)
    (cx2, cy2) (cx3, cy3) (cx4, cy4) alpha name =
  atomic
    [
      num "_c" 0.0;
      str "_p" "textureCropped";
      str "texture" name;
      nums "pos" [ x1; y1; x2; y2; x3; y3; x4; y4 ];
      nums "texc" [ cx1; cy1; cx2; cy2; cx3; cy3; cx4; cy4 ];
      num "alpha" alpha;
    ]

let rec rect_texture_with_alpha (x, y) (w, h) alpha name =
  centered_texture_with_alpha
    (x +. (w /. 2.0), y +. (h /. 2.0))
    (w, h) 0.0 alpha name

and centered_texture_with_alpha (x, y) (w, h) angle alpha name =
  atomic
    [
      num "_c" 0.0;
      str "_p" "centeredTexture";
      str "texture" name;
      nums "posize" [ x; y; w; h ];
      num "angle" angle;
      num "alpha" alpha;
    ]

let rect_texture_cropped_with_alpha (x, y) (w, h) (cx, cy) (cw, ch) alpha name =
  atomic
    [
      num "_c" 0.0;
      str "_p" "textureCropped";
      str "texture" name;
      nums "pos" [ x; y; x +. w; y; x +. w; y +. h; x; y +. h ];
      nums "texc"
        [
          cx;
          1.0 -. cy;
          cx +. cw;
          1.0 -. cy;
          cx +. cw;
          1.0 -. cy -. ch;
          cx;
          1.0 -. cy -. ch;
        ];
      num "alpha" alpha;
    ]

let centered_texture_cropped_with_alpha (x, y) (w, h) angle (cx, cy) (cw, ch)
    alpha name =
  atomic
    [
      num "_c" 0.0;
      str "_p" "centeredCroppedTexture";
      str "texture" name;
      nums "posize" [ x; y; w; h ];
      num "angle" angle;
      nums "texc" [ cx; cy; cw; ch ];
      num "alpha" alpha;
    ]

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

let default_textbox_option =
  {
    fonts = [ "consolas" ];
    text = "";
    size = 24.0;
    color = Color.black;
    word_break = false;
    thickness = None;
    italic = None;
    width = None;
    line_height = None;
    word_spacing = None;
    align = None;
    tab_size = None;
    valign = None;
    letter_spacing = None;
  }

let textbox (x, y) size text font color =
  let rgba_list = to_rgba_list color in
  atomic
    [
      num "_c" 0.0;
      str "_p" "textbox";
      str "text" text;
      num "size" size;
      nums "offset" [ x; y ];
      str "font" font;
      nums "color" rgba_list;
    ]

let textbox_mf (x, y) size text fonts color =
  let rgba_list = to_rgba_list color in
  atomic
    [
      num "_c" 0.0;
      str "_p" "textbox";
      str "text" text;
      num "size" size;
      nums "offset" [ x; y ];
      strs "fonts" fonts;
      nums "color" rgba_list;
    ]

let textbox_centered (x, y) size text font color =
  let rgba_list = to_rgba_list color in
  atomic
    [
      num "_c" 0.0;
      str "_p" "textbox";
      str "text" text;
      num "size" size;
      nums "offset" [ x; y ];
      str "font" font;
      nums "color" rgba_list;
      str "align" "center";
      str "valign" "center";
    ]

let textbox_mf_centered (x, y) size text fonts color =
  let rgba_list = to_rgba_list color in
  atomic
    [
      num "_c" 0.0;
      str "_p" "textbox";
      str "text" text;
      num "size" size;
      nums "offset" [ x; y ];
      strs "fonts" fonts;
      nums "color" rgba_list;
      str "align" "center";
      str "valign" "center";
    ]

let textbox_pro (x, y) opt =
  let rgba_list = to_rgba_list opt.color in
  let get_with_default default = function Some x -> x | None -> default in
  atomic
    [
      num "_c" 0.0;
      str "_p" "textbox";
      str "text" opt.text;
      num "size" opt.size;
      nums "offset" [ x; y ];
      strs "fonts" opt.fonts;
      nums "color" rgba_list;
      bool "wordBreak" opt.word_break;
      str "align" (get_with_default "left" opt.align);
      str "valign" (get_with_default "top" opt.valign);
      num "width" (get_with_default (-1.0) opt.width);
      num "lineHeight" (get_with_default 1.0 opt.line_height);
      num "wordSpacing" (get_with_default 1.0 opt.word_spacing);
      num "letterSpacing" (get_with_default 0.0 opt.letter_spacing);
      num "tabSize" (get_with_default 4.0 opt.tab_size);
      num "thickness" (get_with_default 0.0 opt.thickness);
      num "it" (get_with_default 0.0 opt.italic);
    ]

let save_as_texture text =
  atomic [ num "_c" 4.0; str "_n" text ]
