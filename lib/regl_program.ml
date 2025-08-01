open Js_of_ocaml

type prog_value =
  | DynamicValue of string
  | StaticValue of Js.Unsafe.any
  | DynamicTextureValue of string

type regl_program = {
  frag : string;
  vert : string;
  attributes : (string * prog_value) list option;
  uniforms : (string * prog_value) list option;
  elements : prog_value option;
  primitive : prog_value option;
  count : prog_value option;
}

let get_dynamic_value x =
  let pairs = List.filter_map (fun (k, v) ->
    match v with
    | DynamicValue s -> Some (k, Js.Unsafe.inject (Js.string s))
    | _ -> None
  ) x in
  Js.Unsafe.obj (Array.of_list pairs)

let get_dynamic_texture_value x =
  let pairs = List.filter_map (fun (k, v) ->
    match v with
    | DynamicTextureValue s -> Some (k, Js.Unsafe.inject (Js.string s))
    | _ -> None
  ) x in
  Js.Unsafe.obj (Array.of_list pairs)

let get_static_value x =
  let pairs = List.filter_map (fun (k, v) ->
    match v with
    | StaticValue s -> Some (k, s)
    | _ -> None
  ) x in
  Js.Unsafe.obj (Array.of_list pairs)

let get_static_single_prog_value = function
  | StaticValue s -> s
  | _ -> Js.Unsafe.inject Js.null

let get_dynamic_single_prog_value = function
  | DynamicValue s -> Js.Unsafe.inject (Js.string s)
  | _ -> Js.Unsafe.inject Js.null

let encode_program_helper p =
  let float_array vals = Js.array (Array.of_list (List.map Js.number_of_float vals)) in
  let int_array vals = Js.array (Array.of_list (List.map (fun i -> Js.number_of_float (float_of_int i)) vals)) in
  
  [
    Some ("frag", Js.Unsafe.inject (Js.string p.frag));
    Some ("vert", Js.Unsafe.inject (Js.string p.vert));
    Option.map (fun x -> ("count", get_static_single_prog_value x)) p.count;
    Option.map (fun x -> ("countDyn", get_dynamic_single_prog_value x)) p.count;
    Option.map (fun x -> ("elements", get_static_single_prog_value x)) p.elements;
    Option.map (fun x -> ("elementsDyn", get_dynamic_single_prog_value x)) p.elements;
    Option.map (fun x -> ("primitive", get_static_single_prog_value x)) p.primitive;
    Option.map (fun x -> ("primitiveDyn", get_dynamic_single_prog_value x)) p.primitive;
    Option.map (fun x -> ("attributes", Js.Unsafe.inject (get_static_value x))) p.attributes;
    Option.map (fun x -> ("attributesDyn", Js.Unsafe.inject (get_dynamic_value x))) p.attributes;
    Option.map (fun x -> ("uniforms", Js.Unsafe.inject (get_static_value x))) p.uniforms;
    Option.map (fun x -> ("uniformsDyn", Js.Unsafe.inject (get_dynamic_value x))) p.uniforms;
    Option.map (fun x -> ("uniformsDynTexture", Js.Unsafe.inject (get_dynamic_texture_value x))) p.uniforms;
  ]

let encode_program p =
  let pairs = List.filter_map (fun x -> x) (encode_program_helper p) in
  Js.Unsafe.obj (Array.of_list pairs)

let make_effect_program texname p =
  let new_uniform = match p.uniforms with
    | Some x -> (texname, DynamicValue "texture") :: x
    | None -> [(texname, DynamicValue "texture")]
  in
  { p with uniforms = Some new_uniform }

let make_effect_simple frag uniforms =
  let float_array vals = Js.array (Array.of_list (List.map Js.number_of_float vals)) in
  let int_array vals = Js.array (Array.of_list (List.map (fun i -> Js.number_of_float (float_of_int i)) vals)) in
  
  {
    frag = frag;
    vert = "precision mediump float; attribute vec2 uv; varying vec2 vuv; void main() { vuv = uv; gl_Position = vec4(uv * 2. - 1., 0, 1);}";
    attributes = Some [
      ("uv", StaticValue (Js.Unsafe.inject (float_array [1.0; 1.0; 1.0; 0.0; 0.0; 0.0; 0.0; 1.0])))
    ];
    uniforms = Some (("texture", DynamicValue "texture") :: uniforms);
    elements = Some (StaticValue (Js.Unsafe.inject (int_array [0; 1; 2; 0; 2; 3])));
    primitive = None;
    count = None;
  }

let make_compositor_program src dst p =
  let new_uniform = match p.uniforms with
    | Some x -> (src, DynamicValue "t1") :: (dst, DynamicValue "t2") :: x
    | None -> [(src, DynamicValue "t1"); (dst, DynamicValue "t2")]
  in
  { p with uniforms = Some new_uniform }

let make_compositor_simple frag uniforms =
  let float_array vals = Js.array (Array.of_list (List.map Js.number_of_float vals)) in
  let int_array vals = Js.array (Array.of_list (List.map (fun i -> Js.number_of_float (float_of_int i)) vals)) in
  
  {
    frag = frag;
    vert = "precision mediump float; attribute vec2 uv; varying vec2 vuv; void main() { vuv = uv; gl_Position = vec4(uv * 2. - 1., 0, 1);}";
    attributes = Some [
      ("uv", StaticValue (Js.Unsafe.inject (float_array [1.0; 1.0; 1.0; 0.0; 0.0; 0.0; 0.0; 1.0])))
    ];
    uniforms = Some (("t1", DynamicValue "t1") :: ("t2", DynamicValue "t2") :: uniforms);
    elements = Some (StaticValue (Js.Unsafe.inject (int_array [0; 1; 2; 0; 2; 3])));
    primitive = None;
    count = None;
  }