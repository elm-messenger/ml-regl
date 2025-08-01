open Js_of_ocaml

type time_interval =
  | AnimationFrame
  | Millisecond of float

type regl_config = {
  time_interval : time_interval;
}

type texture_mag_option =
  | MagNearest
  | MagLinear

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

type texture = {
  name : string;
  width : int;
  height : int;
}

type regl_recv_msg =
  | REGLTextureLoaded of texture
  | REGLFontLoaded of string
  | REGLProgramCreated of string

let encode_config config =
  let interval = match config.time_interval with
    | AnimationFrame -> -1.0
    | Millisecond ms -> ms
  in
  (object%js
    val interval = interval
  end)

let encode_texture_options topts =
  match topts with
  | Some opts ->
      let mag_str = match opts.mag with
        | Some MagNearest -> "nearest"
        | Some MagLinear -> "linear"
        | None -> "linear"
      in
      let min_str = match opts.min with
        | Some MinNearest -> "nearest"
        | Some MinLinear -> "linear"
        | Some NearestMipmapNearest -> "nearest mipmap nearest"
        | Some LinearMipmapNearest -> "linear mipmap nearest"
        | Some NearestMipmapLinear -> "nearest mipmap linear"
        | Some LinearMipmapLinear -> "linear mipmap linear"
        | None -> "linear"
      in
      let subimg = match opts.crop with
        | Some ((x, y), (w, h)) ->
            let arr = [|x; y; w; h|] in
            Js.Unsafe.inject (Js.array (Array.map (fun i -> Js.number_of_float (float_of_int i)) arr))
        | None -> Js.Unsafe.inject Js.null
      in
      [
        ("mag", Js.Unsafe.inject (Js.string mag_str));
        ("min", Js.Unsafe.inject (Js.string min_str));
        ("subimg", subimg)
      ]
  | None ->
      [
        ("mag", Js.Unsafe.inject (Js.string "linear"));
        ("min", Js.Unsafe.inject (Js.string "linear"))
      ]

let load_texture name url topts =
  let opts_list = ("data", Js.Unsafe.inject (Js.string url)) :: (encode_texture_options topts) in
  let opts_obj = Js.Unsafe.obj (Array.of_list opts_list) in
  Js.Unsafe.inject (object%js
    val _c = Js.string "loadTexture"
    val _n = Js.string name
    val opts = opts_obj
  end)

let start_regl config =
  match config.builtin_programs with
  | Some progs ->
      let progs_array = Js.array (Array.of_list (List.map Js.string progs)) in
      Js.Unsafe.inject (object%js
        val _c = Js.string "start"
        val virtWidth = config.virt_width
        val virtHeight = config.virt_height
        val fboNum = config.fbo_num
        val programs = progs_array
      end)
  | None ->
      Js.Unsafe.inject (object%js
        val _c = Js.string "start"
        val virtWidth = config.virt_width
        val virtHeight = config.virt_height
        val fboNum = config.fbo_num
      end)

let create_regl_program name program =
  Js.Unsafe.inject (object%js
    val _c = Js.string "createGLProgram"
    val _n = Js.string name
    val proto = Regl_program.encode_program program
  end)

let config_regl config =
  Js.Unsafe.inject (object%js
    val _c = Js.string "config"
    val config = encode_config config
  end)

let load_msdf_font name imgurl jsonurl =
  Js.Unsafe.inject (object%js
    val _c = Js.string "loadFont"
    val _n = Js.string name
    val img = Js.string imgurl
    val json = Js.string jsonurl
  end)

let decode_recv_msg v =
  let get_string_field path =
    try
      Some (Js.to_string (Js.Unsafe.get v (Js.string path)))
    with _ -> None
  in
  let get_int_field path =
    try
      Some (int_of_float (Js.float_of_number (Js.Unsafe.get v (Js.string path))))
    with _ -> None
  in
  match get_string_field "_c" with
  | Some "loadTexture" ->
      (match get_int_field "response.width", get_int_field "response.height", get_string_field "response.texture" with
       | Some w, Some h, Some txtname ->
           Some (REGLTextureLoaded { name = txtname; width = w; height = h })
       | _ -> None)
  | Some "loadFont" ->
      (match get_string_field "response.font" with
       | Some name -> Some (REGLFontLoaded name)
       | None -> None)
  | Some "createGLProgram" ->
      (match get_string_field "response._n" with
       | Some name -> Some (REGLProgramCreated name)
       | None -> None)
  | _ -> None

(* Export functions for js_of_ocaml *)
let () =
  Js.export "REGL"
    (object%js
      method loadTexture name url opts = load_texture name url opts
      method startREGL config = start_regl config
      method createREGLProgram name program = create_regl_program name program
      method configREGL config = config_regl config
      method loadMSDFFont name imgurl jsonurl = load_msdf_font name imgurl jsonurl
      method decodeRecvMsg v = decode_recv_msg v
      method render renderable = Regl_common.render renderable
    end)