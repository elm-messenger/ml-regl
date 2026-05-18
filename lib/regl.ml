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
  let get_string o key =
    try Some (Js.to_string (Js.Unsafe.get o (Js.string key))) with _ -> None
  in
  let get_int o key =
    try
      Some
        (int_of_float (Js.float_of_number (Js.Unsafe.get o (Js.string key))))
    with _ -> None
  in
  match get_string v "_c" with
  | Some "loadTexture" -> (
      let r = Js.Unsafe.get v (Js.string "response") in
      match (get_int r "width", get_int r "height", get_string r "texture") with
      | Some w, Some h, Some txtname ->
          Some (REGLTextureLoaded { name = txtname; width = w; height = h })
      | _ -> None)
  | Some "loadFont" -> (
      let r = Js.Unsafe.get v (Js.string "response") in
      match get_string r "font" with
      | Some name -> Some (REGLFontLoaded name)
      | None -> None)
  | Some "createGLProgram" -> (
      let r = Js.Unsafe.get v (Js.string "response") in
      match get_string r "_n" with
      | Some name -> Some (REGLProgramCreated name)
      | None -> None)
  | _ -> None

let execCmd x =
  let mlregl = Js.Unsafe.global##.MlREGL in
  mlregl##execCmd x

let execAudioCmdPb actions loads =
  let mlregl = Js.Unsafe.global##.MlREGL in
  let payload = Regl_audio.encode_command_batch_pb actions loads in
  let bytes = Regl_transport.uint8array_of_bytes payload in
  Js.Unsafe.fun_call (Js.Unsafe.get mlregl "execAudioCmdPb")
    [| Js.Unsafe.inject bytes |]

let load_audio url = execAudioCmdPb [] [ url ]

type audio_recv_msg =
  | AudioLoadSuccess of { audio_url : string; source : Regl_audio.source }
  | AudioLoadFailed of { audio_url : string; error : Regl_audio.load_error }
  | AudioContextReady of { sample_rate : int }

type regl_input =
  | Tick of float
  | Event of Dom_html.event Js.t
  | REGLRecvMsg of regl_recv_msg
  | AudioMsg of audio_recv_msg

type regl_output =
  | LoadFont of string * string * string
  | LoadTexture of string * string * texture_options option
  | StartREGL of regl_start_config
  | CreateREGLProgram of string * Regl_program.regl_program
  | ConfigREGL of regl_config
  | LoadAudio of string

(* Creating the canvas app. Exposing MlApp. *)
let create_app
    (init : Dom_html.canvasElement Js.t option -> Js.Unsafe.any -> 'a)
    (update :
      Dom_html.canvasElement Js.t option ->
      'a ->
      regl_input ->
      'a * Regl_common.renderable * Regl_audio.audio * regl_output list) =
  let canvas : Dom_html.canvasElement Js.t option ref = ref None in
  let model : 'a option ref = ref None in
  let audio_state : Regl_audio.prev_state ref = ref Regl_audio.empty_state in
  let update_model (input : regl_input) =
    match !model with
    | Some m ->
        let m', rd, audio_tree, outputs = update !canvas m input in
        model := Some m';
        let pending_loads = ref [] in
        List.iter
          (function
            | LoadFont (name, imgurl, jsonurl) ->
                execCmd (load_msdf_font name imgurl jsonurl)
            | LoadTexture (name, url, topts) ->
                execCmd (load_texture name url topts)
            | StartREGL cfg -> execCmd (start_regl cfg)
            | CreateREGLProgram (name, prog) ->
                execCmd (create_regl_program name prog)
            | ConfigREGL cfg -> execCmd (config_regl cfg)
            | LoadAudio url -> pending_loads := url :: !pending_loads)
          outputs;
        let new_state, audio_actions = Regl_audio.diff_actions !audio_state audio_tree in
        audio_state := new_state;
        if audio_actions <> [] || !pending_loads <> [] then
          execAudioCmdPb audio_actions (List.rev !pending_loads);
        Regl_common.render rd
    | None -> Js.Unsafe.inject Js.null
  in
  Js.export "MlApp"
    (Js.Unsafe.obj
       [|
         ("bind", Js.Unsafe.inject (fun c -> canvas := Some c));
         ("init", Js.Unsafe.inject (fun c -> model := Some (init !canvas c)));
         ("update", Js.Unsafe.inject (fun ts -> update_model (Tick ts)));
         ("event", Js.Unsafe.inject (fun ev -> update_model (Event ev)));
         ( "recvREGLCmd",
           Js.Unsafe.inject (fun recvcmd ->
               match decode_recv_msg recvcmd with
               | Some msg -> update_model (REGLRecvMsg msg)
               | None -> Js.Unsafe.inject Js.null) );
        ( "recvAudioMsgPb",
          Js.Unsafe.inject (fun recvmsg ->
              let payload =
                Regl_transport.bytes_of_uint8array
                  (Js.Unsafe.coerce recvmsg)
              in
              match Regl_audio.decode_recv_msg_pb payload with
               | Some (Regl_audio.LoadSuccess { audio_url; source }) ->
                   update_model
                     (AudioMsg (AudioLoadSuccess { audio_url; source }))
               | Some (Regl_audio.LoadFailed { audio_url; error }) ->
                   update_model
                     (AudioMsg (AudioLoadFailed { audio_url; error }))
               | Some (Regl_audio.ContextReady { sample_rate }) ->
                   update_model (AudioMsg (AudioContextReady { sample_rate }))
               | None -> Js.Unsafe.inject Js.null) );
       |])
