open Js_of_ocaml

type time_interval = AnimationFrame | Millisecond of float
type regl_config = { time_interval : time_interval }
type texture_mag_option = MagNearest | MagLinear

type texture_min_option =
  | MinNearest
  | MinLinear
  | NearestMipmapNearest
  | LinearMipmapNearest
  | NearestMipmapLinear
  | LinearMipmapLinear

type texture_options = {
  mag : texture_mag_option option;
  min : texture_min_option option;
  crop : ((int * int) * (int * int)) option;
}

type regl_start_config = {
  virt_width : float;
  virt_height : float;
  fbo_num : int;
  builtin_programs : string list option;
}

type texture = { name : string; width : int; height : int }

type regl_recv_msg =
  | REGLTextureLoaded of texture
  | REGLFontLoaded of string
  | REGLProgramCreated of string

let encode_config config =
  let interval =
    match config.time_interval with
    | AnimationFrame -> -1.0
    | Millisecond ms -> ms
  in
  Js.Unsafe.obj [| ("interval", Js.Unsafe.inject interval) |]

let encode_texture_options topts =
  match topts with
  | Some opts ->
      let mag_str =
        match opts.mag with
        | Some MagNearest -> "nearest"
        | Some MagLinear -> "linear"
        | None -> "linear"
      in
      let min_str =
        match opts.min with
        | Some MinNearest -> "nearest"
        | Some MinLinear -> "linear"
        | Some NearestMipmapNearest -> "nearest mipmap nearest"
        | Some LinearMipmapNearest -> "linear mipmap nearest"
        | Some NearestMipmapLinear -> "nearest mipmap linear"
        | Some LinearMipmapLinear -> "linear mipmap linear"
        | None -> "linear"
      in
      let subimg =
        match opts.crop with
        | Some ((x, y), (w, h)) ->
            let arr = [| x; y; w; h |] in
            Js.Unsafe.inject
              (Js.array
                 (Array.map (fun i -> Js.number_of_float (float_of_int i)) arr))
        | None -> Js.Unsafe.inject Js.null
      in
      [
        ("mag", Js.Unsafe.inject (Js.string mag_str));
        ("min", Js.Unsafe.inject (Js.string min_str));
        ("subimg", subimg);
      ]
  | None ->
      [
        ("mag", Js.Unsafe.inject (Js.string "linear"));
        ("min", Js.Unsafe.inject (Js.string "linear"));
      ]

let load_texture name url topts =
  let opts_list =
    ("data", Js.Unsafe.inject (Js.string url)) :: encode_texture_options topts
  in
  let opts_obj = Js.Unsafe.obj (Array.of_list opts_list) in
  Js.Unsafe.obj
    [|
      ("_c", Js.Unsafe.inject (Js.string "loadTexture"));
      ("_n", Js.Unsafe.inject (Js.string name));
      ("opts", Js.Unsafe.inject opts_obj);
    |]

let start_regl config =
  match config.builtin_programs with
  | Some progs ->
      let progs_array = Js.array (Array.of_list (List.map Js.string progs)) in
      Js.Unsafe.obj
        [|
          ("_c", Js.Unsafe.inject (Js.string "start"));
          ("virtWidth", Js.Unsafe.inject config.virt_width);
          ("virtHeight", Js.Unsafe.inject config.virt_height);
          ("fboNum", Js.Unsafe.inject config.fbo_num);
          ("programs", Js.Unsafe.inject progs_array);
        |]
  | None ->
      Js.Unsafe.obj
        [|
          ("_c", Js.Unsafe.inject (Js.string "start"));
          ("virtWidth", Js.Unsafe.inject config.virt_width);
          ("virtHeight", Js.Unsafe.inject config.virt_height);
          ("fboNum", Js.Unsafe.inject config.fbo_num);
        |]

let create_regl_program name program =
  Js.Unsafe.obj
    [|
      ("_c", Js.Unsafe.inject (Js.string "createGLProgram"));
      ("_n", Js.Unsafe.inject (Js.string name));
      ("proto", Js.Unsafe.inject (Regl_program.encode_program program));
    |]

let config_regl config =
  Js.Unsafe.obj
    [|
      ("_c", Js.Unsafe.inject (Js.string "config"));
      ("config", Js.Unsafe.inject (encode_config config));
    |]

let load_msdf_font name imgurl jsonurl =
  Js.Unsafe.obj
    [|
      ("_c", Js.Unsafe.inject (Js.string "loadFont"));
      ("_n", Js.Unsafe.inject (Js.string name));
      ("img", Js.Unsafe.inject (Js.string imgurl));
      ("json", Js.Unsafe.inject (Js.string jsonurl));
    |]

let decode_recv_msg v =
  let get_string_field path =
    try Some (Js.to_string (Js.Unsafe.get v (Js.string path))) with _ -> None
  in
  let get_int_field path =
    try
      Some
        (int_of_float (Js.float_of_number (Js.Unsafe.get v (Js.string path))))
    with _ -> None
  in
  match get_string_field "_c" with
  | Some "loadTexture" -> (
      match
        ( get_int_field "response.width",
          get_int_field "response.height",
          get_string_field "response.texture" )
      with
      | Some w, Some h, Some txtname ->
          Some (REGLTextureLoaded { name = txtname; width = w; height = h })
      | _ -> None)
  | Some "loadFont" -> (
      match get_string_field "response.font" with
      | Some name -> Some (REGLFontLoaded name)
      | None -> None)
  | Some "createGLProgram" -> (
      match get_string_field "response._n" with
      | Some name -> Some (REGLProgramCreated name)
      | None -> None)
  | _ -> None

(* Export functions for js_of_ocaml *)
let () =
  Js.export "REGL"
    (Js.Unsafe.obj
       [|
         ( "loadTexture",
           Js.Unsafe.inject (fun name url opts -> load_texture name url opts) );
         ("startREGL", Js.Unsafe.inject (fun config -> start_regl config));
         ( "createREGLProgram",
           Js.Unsafe.inject (fun name program ->
               create_regl_program name program) );
         ("configREGL", Js.Unsafe.inject (fun config -> config_regl config));
         ( "loadMSDFFont",
           Js.Unsafe.inject (fun name imgurl jsonurl ->
               load_msdf_font name imgurl jsonurl) );
         ("decodeRecvMsg", Js.Unsafe.inject (fun v -> decode_recv_msg v));
         ( "render",
           Js.Unsafe.inject (fun renderable -> Regl_common.render renderable) );
       |])
