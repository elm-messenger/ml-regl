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

let primitive_to_value = function
  | Points -> Js.Unsafe.inject (Js.string "points")
  | Lines -> Js.Unsafe.inject (Js.string "lines")
  | LineLoop -> Js.Unsafe.inject (Js.string "line loop")
  | LineStrip -> Js.Unsafe.inject (Js.string "line strip")
  | Triangles -> Js.Unsafe.inject (Js.string "triangles")
  | TriangleStrip -> Js.Unsafe.inject (Js.string "triangle strip")
  | TriangleFan -> Js.Unsafe.inject (Js.string "triangle fan")

let empty = gen_prog []

let clear color =
  let rgba_list = to_rgba_list color in
  let color_array =
    Js.array (Array.of_list (List.map Js.number_of_float rgba_list))
  in
  gen_prog
    [
      ("_c", Js.Unsafe.inject (Js.number_of_float 1.0));
      ("_n", Js.Unsafe.inject (Js.string "clear"));
      ("color", Js.Unsafe.inject color_array);
      ("depth", Js.Unsafe.inject (Js.number_of_float 1.0));
    ]

let triangle (x1, y1) (x2, y2) (x3, y3) color =
  let rgba_list = to_rgba_list color in
  let pos_array =
    Js.array
      (Array.of_list (List.map Js.number_of_float [ x1; y1; x2; y2; x3; y3 ]))
  in
  let color_array =
    Js.array (Array.of_list (List.map Js.number_of_float rgba_list))
  in
  gen_prog
    [
      ("_c", Js.Unsafe.inject (Js.number_of_float 0.0));
      ("_p", Js.Unsafe.inject (Js.string "triangle"));
      ("pos", Js.Unsafe.inject pos_array);
      ("color", Js.Unsafe.inject color_array);
    ]

let quad (x1, y1) (x2, y2) (x3, y3) (x4, y4) color =
  let rgba_list = to_rgba_list color in
  let pos_array =
    Js.array
      (Array.of_list
         (List.map Js.number_of_float [ x1; y1; x2; y2; x3; y3; x4; y4 ]))
  in
  let color_array =
    Js.array (Array.of_list (List.map Js.number_of_float rgba_list))
  in
  gen_prog
    [
      ("_c", Js.Unsafe.inject (Js.number_of_float 0.0));
      ("_p", Js.Unsafe.inject (Js.string "quad"));
      ("pos", Js.Unsafe.inject pos_array);
      ("color", Js.Unsafe.inject color_array);
    ]

let rect_centered (x, y) (w, h) angle color =
  let rgba_list = to_rgba_list color in
  let posize_array =
    Js.array (Array.of_list (List.map Js.number_of_float [ x; y; w; h ]))
  in
  let color_array =
    Js.array (Array.of_list (List.map Js.number_of_float rgba_list))
  in
  gen_prog
    [
      ("_c", Js.Unsafe.inject (Js.number_of_float 0.0));
      ("_p", Js.Unsafe.inject (Js.string "rect"));
      ("posize", Js.Unsafe.inject posize_array);
      ("angle", Js.Unsafe.inject (Js.number_of_float angle));
      ("color", Js.Unsafe.inject color_array);
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
  let pos_array = Js.array (Array.of_list (List.map Js.number_of_float pos)) in
  let elem_array =
    Js.array (Array.of_list (List.map Js.number_of_float elem))
  in
  let color_array =
    Js.array (Array.of_list (List.map Js.number_of_float rgba_list))
  in
  gen_prog
    [
      ("_c", Js.Unsafe.inject (Js.number_of_float 0.0));
      ("_p", Js.Unsafe.inject (Js.string "poly"));
      ("pos", Js.Unsafe.inject pos_array);
      ("elem", Js.Unsafe.inject elem_array);
      ("color", Js.Unsafe.inject color_array);
    ]

let lines xs color =
  let pos =
    List.concat_map (fun ((x1, y1), (x2, y2)) -> [ x1; y1; x2; y2 ]) xs
  in
  let elem = List.init (2 * List.length xs) float_of_int in
  let rgba_list = to_rgba_list color in
  let pos_array = Js.array (Array.of_list (List.map Js.number_of_float pos)) in
  let elem_array =
    Js.array (Array.of_list (List.map Js.number_of_float elem))
  in
  let color_array =
    Js.array (Array.of_list (List.map Js.number_of_float rgba_list))
  in
  gen_prog
    [
      ("_c", Js.Unsafe.inject (Js.number_of_float 0.0));
      ("_p", Js.Unsafe.inject (Js.string "poly"));
      ("pos", Js.Unsafe.inject pos_array);
      ("elem", Js.Unsafe.inject elem_array);
      ("color", Js.Unsafe.inject color_array);
      ("prim", primitive_to_value Lines);
    ]

let linestrip xs color =
  let pos = List.concat_map (fun (x, y) -> [ x; y ]) xs in
  let elem = List.init (List.length xs) float_of_int in
  let rgba_list = to_rgba_list color in
  let pos_array = Js.array (Array.of_list (List.map Js.number_of_float pos)) in
  let elem_array =
    Js.array (Array.of_list (List.map Js.number_of_float elem))
  in
  let color_array =
    Js.array (Array.of_list (List.map Js.number_of_float rgba_list))
  in
  gen_prog
    [
      ("_c", Js.Unsafe.inject (Js.number_of_float 0.0));
      ("_p", Js.Unsafe.inject (Js.string "poly"));
      ("pos", Js.Unsafe.inject pos_array);
      ("elem", Js.Unsafe.inject elem_array);
      ("color", Js.Unsafe.inject color_array);
      ("prim", primitive_to_value LineStrip);
    ]

let lineloop xs color =
  let pos = List.concat_map (fun (x, y) -> [ x; y ]) xs in
  let elem = List.init (List.length xs) float_of_int in
  let rgba_list = to_rgba_list color in
  let pos_array = Js.array (Array.of_list (List.map Js.number_of_float pos)) in
  let elem_array =
    Js.array (Array.of_list (List.map Js.number_of_float elem))
  in
  let color_array =
    Js.array (Array.of_list (List.map Js.number_of_float rgba_list))
  in
  gen_prog
    [
      ("_c", Js.Unsafe.inject (Js.number_of_float 0.0));
      ("_p", Js.Unsafe.inject (Js.string "poly"));
      ("pos", Js.Unsafe.inject pos_array);
      ("elem", Js.Unsafe.inject elem_array);
      ("color", Js.Unsafe.inject color_array);
      ("prim", primitive_to_value LineLoop);
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
  let pos_array = Js.array (Array.of_list (List.map Js.number_of_float pos)) in
  let elem_array =
    Js.array (Array.of_list (List.map Js.number_of_float elem))
  in
  let color_array =
    Js.array (Array.of_list (List.map Js.number_of_float rgba_list))
  in
  gen_prog
    [
      ("_c", Js.Unsafe.inject (Js.number_of_float 0.0));
      ("_p", Js.Unsafe.inject (Js.string "poly"));
      ("pos", Js.Unsafe.inject pos_array);
      ("elem", Js.Unsafe.inject elem_array);
      ("color", Js.Unsafe.inject color_array);
      ("prim", primitive_to_value prim);
    ]

let circle (x1, y1) r color =
  let rgba_list = to_rgba_list color in
  let cr_array =
    Js.array (Array.of_list (List.map Js.number_of_float [ x1; y1; r ]))
  in
  let color_array =
    Js.array (Array.of_list (List.map Js.number_of_float rgba_list))
  in
  gen_prog
    [
      ("_c", Js.Unsafe.inject (Js.number_of_float 0.0));
      ("_p", Js.Unsafe.inject (Js.string "circle"));
      ("cr", Js.Unsafe.inject cr_array);
      ("color", Js.Unsafe.inject color_array);
    ]

let rounded_rect (x1, y1) (w, h) r color =
  let rgba_list = to_rgba_list color in
  let cs_array =
    Js.array (Array.of_list (List.map Js.number_of_float [ x1; y1; w; h ]))
  in
  let color_array =
    Js.array (Array.of_list (List.map Js.number_of_float rgba_list))
  in
  gen_prog
    [
      ("_c", Js.Unsafe.inject (Js.number_of_float 0.0));
      ("_p", Js.Unsafe.inject (Js.string "roundedRect"));
      ("cs", Js.Unsafe.inject cs_array);
      ("radius", Js.Unsafe.inject (Js.number_of_float r));
      ("color", Js.Unsafe.inject color_array);
    ]

let texture (x1, y1) (x2, y2) (x3, y3) (x4, y4) name =
  let pos_array =
    Js.array
      (Array.of_list
         (List.map Js.number_of_float [ x1; y1; x2; y2; x3; y3; x4; y4 ]))
  in
  gen_prog
    [
      ("_c", Js.Unsafe.inject (Js.number_of_float 0.0));
      ("_p", Js.Unsafe.inject (Js.string "texture"));
      ("texture", Js.Unsafe.inject (Js.string name));
      ("pos", Js.Unsafe.inject pos_array);
    ]

let texture_cropped (x1, y1) (x2, y2) (x3, y3) (x4, y4) (cx1, cy1) (cx2, cy2)
    (cx3, cy3) (cx4, cy4) name =
  let pos_array =
    Js.array
      (Array.of_list
         (List.map Js.number_of_float [ x1; y1; x2; y2; x3; y3; x4; y4 ]))
  in
  let texc_array =
    Js.array
      (Array.of_list
         (List.map Js.number_of_float
            [ cx1; cy1; cx2; cy2; cx3; cy3; cx4; cy4 ]))
  in
  gen_prog
    [
      ("_c", Js.Unsafe.inject (Js.number_of_float 0.0));
      ("_p", Js.Unsafe.inject (Js.string "textureCropped"));
      ("texture", Js.Unsafe.inject (Js.string name));
      ("pos", Js.Unsafe.inject pos_array);
      ("texc", Js.Unsafe.inject texc_array);
    ]

let rec rect_texture (x, y) (w, h) name =
  centered_texture (x +. (w /. 2.0), y +. (h /. 2.0)) (w, h) 0.0 name

and centered_texture (x, y) (w, h) angle name =
  let posize_array =
    Js.array (Array.of_list (List.map Js.number_of_float [ x; y; w; h ]))
  in
  gen_prog
    [
      ("_c", Js.Unsafe.inject (Js.number_of_float 0.0));
      ("_p", Js.Unsafe.inject (Js.string "centeredTexture"));
      ("texture", Js.Unsafe.inject (Js.string name));
      ("posize", Js.Unsafe.inject posize_array);
      ("angle", Js.Unsafe.inject (Js.number_of_float angle));
    ]

let rect_texture_cropped (x, y) (w, h) (cx, cy) (cw, ch) name =
  let pos_array =
    Js.array
      (Array.of_list
         (List.map Js.number_of_float
            [ x; y; x +. w; y; x +. w; y +. h; x; y +. h ]))
  in
  let texc_array =
    Js.array
      (Array.of_list
         (List.map Js.number_of_float
            [
              cx;
              1.0 -. cy;
              cx +. cw;
              1.0 -. cy;
              cx +. cw;
              1.0 -. cy -. ch;
              cx;
              1.0 -. cy -. ch;
            ]))
  in
  gen_prog
    [
      ("_c", Js.Unsafe.inject (Js.number_of_float 0.0));
      ("_p", Js.Unsafe.inject (Js.string "textureCropped"));
      ("texture", Js.Unsafe.inject (Js.string name));
      ("pos", Js.Unsafe.inject pos_array);
      ("texc", Js.Unsafe.inject texc_array);
    ]

let centered_texture_cropped (x, y) (w, h) angle (cx, cy) (cw, ch) name =
  let posize_array =
    Js.array (Array.of_list (List.map Js.number_of_float [ x; y; w; h ]))
  in
  let texc_array =
    Js.array (Array.of_list (List.map Js.number_of_float [ cx; cy; cw; ch ]))
  in
  gen_prog
    [
      ("_c", Js.Unsafe.inject (Js.number_of_float 0.0));
      ("_p", Js.Unsafe.inject (Js.string "centeredCroppedTexture"));
      ("texture", Js.Unsafe.inject (Js.string name));
      ("posize", Js.Unsafe.inject posize_array);
      ("angle", Js.Unsafe.inject (Js.number_of_float angle));
      ("texc", Js.Unsafe.inject texc_array);
    ]

(* Functions with alpha *)
let texture_with_alpha (x1, y1) (x2, y2) (x3, y3) (x4, y4) alpha name =
  let pos_array =
    Js.array
      (Array.of_list
         (List.map Js.number_of_float [ x1; y1; x2; y2; x3; y3; x4; y4 ]))
  in
  gen_prog
    [
      ("_c", Js.Unsafe.inject (Js.number_of_float 0.0));
      ("_p", Js.Unsafe.inject (Js.string "texture"));
      ("texture", Js.Unsafe.inject (Js.string name));
      ("pos", Js.Unsafe.inject pos_array);
      ("alpha", Js.Unsafe.inject (Js.number_of_float alpha));
    ]

let texture_cropped_with_alpha (x1, y1) (x2, y2) (x3, y3) (x4, y4) (cx1, cy1)
    (cx2, cy2) (cx3, cy3) (cx4, cy4) alpha name =
  let pos_array =
    Js.array
      (Array.of_list
         (List.map Js.number_of_float [ x1; y1; x2; y2; x3; y3; x4; y4 ]))
  in
  let texc_array =
    Js.array
      (Array.of_list
         (List.map Js.number_of_float
            [ cx1; cy1; cx2; cy2; cx3; cy3; cx4; cy4 ]))
  in
  gen_prog
    [
      ("_c", Js.Unsafe.inject (Js.number_of_float 0.0));
      ("_p", Js.Unsafe.inject (Js.string "textureCropped"));
      ("texture", Js.Unsafe.inject (Js.string name));
      ("pos", Js.Unsafe.inject pos_array);
      ("texc", Js.Unsafe.inject texc_array);
      ("alpha", Js.Unsafe.inject (Js.number_of_float alpha));
    ]

let rec rect_texture_with_alpha (x, y) (w, h) alpha name =
  centered_texture_with_alpha
    (x +. (w /. 2.0), y +. (h /. 2.0))
    (w, h) 0.0 alpha name

and centered_texture_with_alpha (x, y) (w, h) angle alpha name =
  let posize_array =
    Js.array (Array.of_list (List.map Js.number_of_float [ x; y; w; h ]))
  in
  gen_prog
    [
      ("_c", Js.Unsafe.inject (Js.number_of_float 0.0));
      ("_p", Js.Unsafe.inject (Js.string "centeredTexture"));
      ("texture", Js.Unsafe.inject (Js.string name));
      ("posize", Js.Unsafe.inject posize_array);
      ("angle", Js.Unsafe.inject (Js.number_of_float angle));
      ("alpha", Js.Unsafe.inject (Js.number_of_float alpha));
    ]

let rect_texture_cropped_with_alpha (x, y) (w, h) (cx, cy) (cw, ch) alpha name =
  let pos_array =
    Js.array
      (Array.of_list
         (List.map Js.number_of_float
            [ x; y; x +. w; y; x +. w; y +. h; x; y +. h ]))
  in
  let texc_array =
    Js.array
      (Array.of_list
         (List.map Js.number_of_float
            [
              cx;
              1.0 -. cy;
              cx +. cw;
              1.0 -. cy;
              cx +. cw;
              1.0 -. cy -. ch;
              cx;
              1.0 -. cy -. ch;
            ]))
  in
  gen_prog
    [
      ("_c", Js.Unsafe.inject (Js.number_of_float 0.0));
      ("_p", Js.Unsafe.inject (Js.string "textureCropped"));
      ("texture", Js.Unsafe.inject (Js.string name));
      ("pos", Js.Unsafe.inject pos_array);
      ("texc", Js.Unsafe.inject texc_array);
      ("alpha", Js.Unsafe.inject (Js.number_of_float alpha));
    ]

let centered_texture_cropped_with_alpha (x, y) (w, h) angle (cx, cy) (cw, ch)
    alpha name =
  let posize_array =
    Js.array (Array.of_list (List.map Js.number_of_float [ x; y; w; h ]))
  in
  let texc_array =
    Js.array (Array.of_list (List.map Js.number_of_float [ cx; cy; cw; ch ]))
  in
  gen_prog
    [
      ("_c", Js.Unsafe.inject (Js.number_of_float 0.0));
      ("_p", Js.Unsafe.inject (Js.string "centeredCroppedTexture"));
      ("texture", Js.Unsafe.inject (Js.string name));
      ("posize", Js.Unsafe.inject posize_array);
      ("angle", Js.Unsafe.inject (Js.number_of_float angle));
      ("texc", Js.Unsafe.inject texc_array);
      ("alpha", Js.Unsafe.inject (Js.number_of_float alpha));
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
  let offset_array =
    Js.array (Array.of_list (List.map Js.number_of_float [ x; y ]))
  in
  let color_array =
    Js.array (Array.of_list (List.map Js.number_of_float rgba_list))
  in
  gen_prog
    [
      ("_c", Js.Unsafe.inject (Js.number_of_float 0.0));
      ("_p", Js.Unsafe.inject (Js.string "textbox"));
      ("text", Js.Unsafe.inject (Js.string text));
      ("size", Js.Unsafe.inject (Js.number_of_float size));
      ("offset", Js.Unsafe.inject offset_array);
      ("font", Js.Unsafe.inject (Js.string font));
      ("color", Js.Unsafe.inject color_array);
    ]

let textbox_mf (x, y) size text fonts color =
  let rgba_list = to_rgba_list color in
  let offset_array =
    Js.array (Array.of_list (List.map Js.number_of_float [ x; y ]))
  in
  let fonts_array = Js.array (Array.of_list (List.map Js.string fonts)) in
  let color_array =
    Js.array (Array.of_list (List.map Js.number_of_float rgba_list))
  in
  gen_prog
    [
      ("_c", Js.Unsafe.inject (Js.number_of_float 0.0));
      ("_p", Js.Unsafe.inject (Js.string "textbox"));
      ("text", Js.Unsafe.inject (Js.string text));
      ("size", Js.Unsafe.inject (Js.number_of_float size));
      ("offset", Js.Unsafe.inject offset_array);
      ("fonts", Js.Unsafe.inject fonts_array);
      ("color", Js.Unsafe.inject color_array);
    ]

let textbox_centered (x, y) size text font color =
  let rgba_list = to_rgba_list color in
  let offset_array =
    Js.array (Array.of_list (List.map Js.number_of_float [ x; y ]))
  in
  let color_array =
    Js.array (Array.of_list (List.map Js.number_of_float rgba_list))
  in
  gen_prog
    [
      ("_c", Js.Unsafe.inject (Js.number_of_float 0.0));
      ("_p", Js.Unsafe.inject (Js.string "textbox"));
      ("text", Js.Unsafe.inject (Js.string text));
      ("size", Js.Unsafe.inject (Js.number_of_float size));
      ("offset", Js.Unsafe.inject offset_array);
      ("font", Js.Unsafe.inject (Js.string font));
      ("color", Js.Unsafe.inject color_array);
      ("align", Js.Unsafe.inject (Js.string "center"));
      ("valign", Js.Unsafe.inject (Js.string "center"));
    ]

let textbox_mf_centered (x, y) size text fonts color =
  let rgba_list = to_rgba_list color in
  let offset_array =
    Js.array (Array.of_list (List.map Js.number_of_float [ x; y ]))
  in
  let fonts_array = Js.array (Array.of_list (List.map Js.string fonts)) in
  let color_array =
    Js.array (Array.of_list (List.map Js.number_of_float rgba_list))
  in
  gen_prog
    [
      ("_c", Js.Unsafe.inject (Js.number_of_float 0.0));
      ("_p", Js.Unsafe.inject (Js.string "textbox"));
      ("text", Js.Unsafe.inject (Js.string text));
      ("size", Js.Unsafe.inject (Js.number_of_float size));
      ("offset", Js.Unsafe.inject offset_array);
      ("fonts", Js.Unsafe.inject fonts_array);
      ("color", Js.Unsafe.inject color_array);
      ("align", Js.Unsafe.inject (Js.string "center"));
      ("valign", Js.Unsafe.inject (Js.string "center"));
    ]

let textbox_pro (x, y) opt =
  let rgba_list = to_rgba_list opt.color in
  let offset_array =
    Js.array (Array.of_list (List.map Js.number_of_float [ x; y ]))
  in
  let fonts_array = Js.array (Array.of_list (List.map Js.string opt.fonts)) in
  let color_array =
    Js.array (Array.of_list (List.map Js.number_of_float rgba_list))
  in
  let get_with_default default = function Some x -> x | None -> default in
  gen_prog
    [
      ("_c", Js.Unsafe.inject (Js.number_of_float 0.0));
      ("_p", Js.Unsafe.inject (Js.string "textbox"));
      ("text", Js.Unsafe.inject (Js.string opt.text));
      ("size", Js.Unsafe.inject (Js.number_of_float opt.size));
      ("offset", Js.Unsafe.inject offset_array);
      ("fonts", Js.Unsafe.inject fonts_array);
      ("color", Js.Unsafe.inject color_array);
      ("wordBreak", Js.Unsafe.inject (Js.bool opt.word_break));
      ("align", Js.Unsafe.inject (Js.string (get_with_default "left" opt.align)));
      ( "valign",
        Js.Unsafe.inject (Js.string (get_with_default "top" opt.valign)) );
      ( "width",
        Js.Unsafe.inject
          (Js.number_of_float (get_with_default (-1.0) opt.width)) );
      ( "lineHeight",
        Js.Unsafe.inject
          (Js.number_of_float (get_with_default 1.0 opt.line_height)) );
      ( "wordSpacing",
        Js.Unsafe.inject
          (Js.number_of_float (get_with_default 1.0 opt.word_spacing)) );
      ( "letterSpacing",
        Js.Unsafe.inject
          (Js.number_of_float (get_with_default 0.0 opt.letter_spacing)) );
      ( "tabSize",
        Js.Unsafe.inject
          (Js.number_of_float (get_with_default 4.0 opt.tab_size)) );
      ( "thickness",
        Js.Unsafe.inject
          (Js.number_of_float (get_with_default 0.0 opt.thickness)) );
      ( "it",
        Js.Unsafe.inject (Js.number_of_float (get_with_default 0.0 opt.italic))
      );
    ]

let save_as_texture text =
  gen_prog
    [
      ("_c", Js.Unsafe.inject (Js.number_of_float 4.0));
      ("_n", Js.Unsafe.inject (Js.string text));
    ]
