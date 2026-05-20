module Backend_pb = Transport_backend.Mlregl.Transport.Backend
module Common_pb = Transport_common.Mlregl.Transport.Common

type prog_value =
  | DynamicValue of string
  | StaticValue of Common_pb.Value.t
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

let static_number n =
  StaticValue (Common_pb.Value.make ~kind:(`Number_value n) ())

let static_string s =
  StaticValue (Common_pb.Value.make ~kind:(`String_value s) ())

let static_bool b =
  StaticValue (Common_pb.Value.make ~kind:(`Bool_value b) ())

let static_numbers values =
  let arr = Common_pb.ScalarArray.make ~values () in
  StaticValue (Common_pb.Value.make ~kind:(`Number_array_value arr) ())

let static_strings values =
  let arr = Common_pb.StringArray.make ~values () in
  StaticValue (Common_pb.Value.make ~kind:(`String_array_value arr) ())

let encode_program_value = function
  | DynamicValue s -> Some (Backend_pb.ProgramValue.make ~val':(`Dyn_val s) ())
  | DynamicTextureValue s ->
      Some (Backend_pb.ProgramValue.make ~val':(`Dyn_textval s) ())
  | StaticValue v ->
      Some (Backend_pb.ProgramValue.make ~val':(`Static_val v) ())

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
  {
    frag;
    vert =
      "precision mediump float; attribute vec2 uv; varying vec2 vuv; void \
       main() { vuv = uv; gl_Position = vec4(uv * 2. - 1., 0, 1);}";
    attributes =
      Some
        [
          ( "uv",
            static_numbers [ 1.0; 1.0; 1.0; 0.0; 0.0; 0.0; 0.0; 1.0 ] );
        ];
    uniforms = Some (("texture", DynamicValue "texture") :: uniforms);
    elements =
      Some (static_numbers [ 0.0; 1.0; 2.0; 0.0; 2.0; 3.0 ]);
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
  {
    frag;
    vert =
      "precision mediump float; attribute vec2 uv; varying vec2 vuv; void \
       main() { vuv = uv; gl_Position = vec4(uv * 2. - 1., 0, 1);}";
    attributes =
      Some
        [
          ( "uv",
            static_numbers [ 1.0; 1.0; 1.0; 0.0; 0.0; 0.0; 0.0; 1.0 ] );
        ];
    uniforms =
      Some (("t1", DynamicValue "t1") :: ("t2", DynamicValue "t2") :: uniforms);
    elements =
      Some (static_numbers [ 0.0; 1.0; 2.0; 0.0; 2.0; 3.0 ]);
    primitive = None;
    count = None;
  }
