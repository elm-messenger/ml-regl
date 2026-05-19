open Js_of_ocaml
module Backend_pb = Transport_backend.Mlregl.Transport.Backend
module Common_pb = Transport_common.Mlregl.Transport.Common

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

let js_typeof x = Js.to_string (Js.typeof x)

let js_array_is_array =
  Js.Unsafe.get (Js.Unsafe.js_expr "Array") "isArray"

let is_js_array x =
  Js.to_bool
    (Js.Unsafe.fun_call js_array_is_array [| Js.Unsafe.inject x |])

let common_number n = Common_pb.Value.make ~kind:(`Number_value n) ()
let common_string s = Common_pb.Value.make ~kind:(`String_value s) ()
let common_bool b = Common_pb.Value.make ~kind:(`Bool_value b) ()

let common_number_array values =
  let arr = Common_pb.ScalarArray.make ~values () in
  Common_pb.Value.make ~kind:(`Number_array_value arr) ()

let common_string_array values =
  let arr = Common_pb.StringArray.make ~values () in
  Common_pb.Value.make ~kind:(`String_array_value arr) ()

let static_value_to_common (v : Js.Unsafe.any) : Common_pb.Value.t option =
  match js_typeof v with
  | "number" ->
      Some (common_number (Js.float_of_number (Js.Unsafe.coerce v)))
  | "string" -> Some (common_string (Js.to_string (Js.Unsafe.coerce v)))
  | "boolean" -> Some (common_bool (Js.to_bool (Js.Unsafe.coerce v)))
  | _ when is_js_array v ->
      let arr = Js.to_array (Js.Unsafe.coerce v) in
      let values = Array.to_list arr in
      let rec all_numbers acc = function
        | [] -> Some (List.rev acc)
        | x :: rest when js_typeof x = "number" ->
            all_numbers (Js.float_of_number (Js.Unsafe.coerce x) :: acc) rest
        | _ -> None
      in
      let rec all_strings acc = function
        | [] -> Some (List.rev acc)
        | x :: rest when js_typeof x = "string" ->
            all_strings (Js.to_string (Js.Unsafe.coerce x) :: acc) rest
        | _ -> None
      in
      (match all_numbers [] values with
      | Some xs -> Some (common_number_array xs)
      | None -> (
          match all_strings [] values with
          | Some xs -> Some (common_string_array xs)
          | None -> None))
  | _ -> None

let encode_program_value = function
  | DynamicValue s -> Some (Backend_pb.ProgramValue.make ~val':(`Dyn_val s) ())
  | DynamicTextureValue s ->
      Some (Backend_pb.ProgramValue.make ~val':(`Dyn_textval s) ())
  | StaticValue v ->
      Option.map
        (fun sv -> Backend_pb.ProgramValue.make ~val':(`Static_val sv) ())
        (static_value_to_common v)

let encode_mapping (key, value) =
  Option.map
    (fun v -> Backend_pb.ProgramValueMapping.make ~key ~val':v ())
    (encode_program_value value)

let encode_mapping_list = function
  | None -> []
  | Some xs -> List.filter_map encode_mapping xs

let encode_program_pb p =
  Backend_pb.Program.make ~frag:p.frag ~vert:p.vert
    ~uniforms:(encode_mapping_list p.uniforms)
    ~attributes:(encode_mapping_list p.attributes)
    ?primitive:(Option.bind p.primitive encode_program_value)
    ?elements:(Option.bind p.elements encode_program_value)
    ?count:(Option.bind p.count encode_program_value)
    ()

let make_effect_program texname p =
  let new_uniform =
    match p.uniforms with
    | Some x -> (texname, DynamicValue "texture") :: x
    | None -> [ (texname, DynamicValue "texture") ]
  in
  { p with uniforms = Some new_uniform }

let make_effect_simple frag uniforms =
  let float_array vals =
    Js.array (Array.of_list (List.map Js.number_of_float vals))
  in
  let int_array vals =
    Js.array
      (Array.of_list
         (List.map (fun i -> Js.number_of_float (float_of_int i)) vals))
  in

  {
    frag;
    vert =
      "precision mediump float; attribute vec2 uv; varying vec2 vuv; void \
       main() { vuv = uv; gl_Position = vec4(uv * 2. - 1., 0, 1);}";
    attributes =
      Some
        [
          ( "uv",
            StaticValue
              (Js.Unsafe.inject
                 (float_array [ 1.0; 1.0; 1.0; 0.0; 0.0; 0.0; 0.0; 1.0 ])) );
        ];
    uniforms = Some (("texture", DynamicValue "texture") :: uniforms);
    elements =
      Some (StaticValue (Js.Unsafe.inject (int_array [ 0; 1; 2; 0; 2; 3 ])));
    primitive = None;
    count = None;
  }

let make_compositor_program src dst p =
  let new_uniform =
    match p.uniforms with
    | Some x -> (src, DynamicValue "t1") :: (dst, DynamicValue "t2") :: x
    | None -> [ (src, DynamicValue "t1"); (dst, DynamicValue "t2") ]
  in
  { p with uniforms = Some new_uniform }

let make_compositor_simple frag uniforms =
  let float_array vals =
    Js.array (Array.of_list (List.map Js.number_of_float vals))
  in
  let int_array vals =
    Js.array
      (Array.of_list
         (List.map (fun i -> Js.number_of_float (float_of_int i)) vals))
  in

  {
    frag;
    vert =
      "precision mediump float; attribute vec2 uv; varying vec2 vuv; void \
       main() { vuv = uv; gl_Position = vec4(uv * 2. - 1., 0, 1);}";
    attributes =
      Some
        [
          ( "uv",
            StaticValue
              (Js.Unsafe.inject
                 (float_array [ 1.0; 1.0; 1.0; 0.0; 0.0; 0.0; 0.0; 1.0 ])) );
        ];
    uniforms =
      Some (("t1", DynamicValue "t1") :: ("t2", DynamicValue "t2") :: uniforms);
    elements =
      Some (StaticValue (Js.Unsafe.inject (int_array [ 0; 1; 2; 0; 2; 3 ])));
    primitive = None;
    count = None;
  }
