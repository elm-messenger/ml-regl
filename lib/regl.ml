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

module Backend_pb = Transport_backend.Mlregl.Transport.Backend
module Common_pb = Transport_common.Mlregl.Transport.Common

let backend_mag = function
  | MagNearest -> Backend_pb.TextureMagOption.TEXTURE_MAG_OPTION_NEAREST
  | MagLinear -> Backend_pb.TextureMagOption.TEXTURE_MAG_OPTION_LINEAR

let backend_min = function
  | MinNearest -> Backend_pb.TextureMinOption.TEXTURE_MIN_OPTION_NEAREST
  | MinLinear -> Backend_pb.TextureMinOption.TEXTURE_MIN_OPTION_LINEAR
  | NearestMipmapNearest ->
      Backend_pb.TextureMinOption.TEXTURE_MIN_OPTION_NEAREST_MIPMAP_NEAREST
  | LinearMipmapNearest ->
      Backend_pb.TextureMinOption.TEXTURE_MIN_OPTION_LINEAR_MIPMAP_NEAREST
  | NearestMipmapLinear ->
      Backend_pb.TextureMinOption.TEXTURE_MIN_OPTION_NEAREST_MIPMAP_LINEAR
  | LinearMipmapLinear ->
      Backend_pb.TextureMinOption.TEXTURE_MIN_OPTION_LINEAR_MIPMAP_LINEAR

let backend_texture_options = function
  | None -> None
  | Some { mag; min; crop } ->
      let mag =
        match mag with
        | None -> Backend_pb.TextureMagOption.TEXTURE_MAG_OPTION_LINEAR
        | Some m -> backend_mag m
      in
      let min =
        match min with
        | None -> Backend_pb.TextureMinOption.TEXTURE_MIN_OPTION_LINEAR
        | Some m -> backend_min m
      in
      let crop =
        match crop with
        | None -> None
        | Some ((x, y), (w, h)) ->
            Some (Backend_pb.TextureCrop.make ~x ~y ~width:w ~height:h ())
      in
      Some (Backend_pb.TextureOptions.make ~mag ~min ?crop ())

let backend_create_program name program =
  Backend_pb.CreateProgram.make ~name
    ~program:(Regl_program.encode_program_pb program)
    ()

let encode_backend_command_batch_pb
    (commands : Backend_pb.BackendCommand.t list) : bytes =
  Backend_pb.BackendCommandBatch.to_proto commands
  |> Ocaml_protoc_plugin.Writer.contents |> Bytes.unsafe_of_string

let execCmdPb commands =
  let mlregl = Js.Unsafe.global##.MlREGL in
  let payload = encode_backend_command_batch_pb commands in
  let bytes = Regl_transport.uint8array_of_bytes payload in
  Js.Unsafe.fun_call
    (Js.Unsafe.get mlregl "execCmdPb")
    [| Js.Unsafe.inject bytes |]

let decode_backend_event_pb (payload : bytes) : regl_recv_msg option =
  try
    let reader =
      Ocaml_protoc_plugin.Reader.create (Bytes.unsafe_to_string payload)
    in
    match Backend_pb.BackendEvent.from_proto_exn reader with
    | `Texture_loaded { name; width; height } ->
        Some (REGLTextureLoaded { name; width; height })
    | `Font_loaded name -> Some (REGLFontLoaded name)
    | `Program_created name -> Some (REGLProgramCreated name)
    | `not_set -> None
  with _ -> None

let execAudioCmdPb actions =
  let mlregl = Js.Unsafe.global##.MlREGL in
  let payload = Regl_audio.encode_command_batch_pb actions in
  let bytes = Regl_transport.uint8array_of_bytes payload in
  Js.Unsafe.fun_call
    (Js.Unsafe.get mlregl "execAudioCmdPb")
    [| Js.Unsafe.inject bytes |]

type audio_recv_msg =
  | AudioLoadSuccess of { audio_url : string; source : Regl_audio.source }
  | AudioLoadFailed of { audio_url : string; error : Regl_audio.load_error }
  | AudioContextReady of { sample_rate : int }

type regl_input =
  | Tick of float
  | Event of Dom_html.event Js.t
  | REGLRecvMsg of regl_recv_msg
  | AudioMsg of audio_recv_msg

type regl_output = Backend_pb.BackendCommand.t

let load_texture name url options =
  Backend_pb.BackendCommand.make
    ~kind:
      (`Load_texture
         (Backend_pb.LoadTexture.make ~name ~url
            ?options:(backend_texture_options options)
            ()))
    ()

let load_font name image_url json_url =
  Backend_pb.BackendCommand.make
    ~kind:(`Load_font (Backend_pb.LoadFont.make ~name ~image_url ~json_url ()))
    ()

let start_regl cfg =
  Backend_pb.BackendCommand.make
    ~kind:
      (match cfg.builtin_programs with
      | None ->
          `Start_regl
            (Backend_pb.StartRegl.make ~virt_width:cfg.virt_width
               ~virt_height:cfg.virt_height ~fbo_num:cfg.fbo_num ())
      | Some xs ->
          let bps = Common_pb.StringArray.make ~values:xs () in
          `Start_regl
            (Backend_pb.StartRegl.make ~virt_width:cfg.virt_width
               ~virt_height:cfg.virt_height ~fbo_num:cfg.fbo_num
               ~builtin_programs:bps ()))
    ()

let create_regl_program name program =
  Backend_pb.BackendCommand.make
    ~kind:(`Create_program (backend_create_program name program))
    ()

let config_regl cfg =
  let interval_ms =
    match cfg.time_interval with AnimationFrame -> -1.0 | Millisecond ms -> ms
  in
  Backend_pb.BackendCommand.make
    ~kind:(`Config_regl (Backend_pb.ReglConfig.make ~interval_ms ()))
    ()

let load_audio audio_url =
  Backend_pb.BackendCommand.make
    ~kind:(`Load_audio (Backend_pb.LoadAudio.make ~audio_url ()))
    ()

(* Creating the canvas app. Exposing MlApp. *)
let create_app
    (init : Dom_html.canvasElement Js.t option -> 'a * regl_output list)
    (update :
      Dom_html.canvasElement Js.t option ->
      'a ->
      regl_input ->
      'a * Regl_audio.audio * regl_output list)
    (view : 'a -> Regl_common.renderable) =
  let canvas : Dom_html.canvasElement Js.t option ref = ref None in
  let model : 'a option ref = ref None in
  let audio_state : Regl_audio.prev_state ref = ref Regl_audio.empty_state in
  let execute_backend_cmds cmds = if cmds <> [] then execCmdPb cmds in
  let update_model (input : regl_input) =
    match !model with
    | Some m ->
        let m', audio_tree, outputs = update !canvas m input in
        model := Some m';
        execute_backend_cmds outputs;
        let new_state, audio_actions =
          Regl_audio.diff_actions !audio_state audio_tree
        in
        audio_state := new_state;
        if audio_actions <> [] then execAudioCmdPb audio_actions
    | None -> ()
  in
  Js.export "MlApp"
    (Js.Unsafe.obj
       [|
         ("bind", Js.Unsafe.inject (fun c -> canvas := Some c));
         ( "init",
           Js.Unsafe.inject (fun _ ->
               let m, outputs = init !canvas in
               model := Some m;
               execute_backend_cmds outputs) );
         ("update", Js.Unsafe.inject (fun ts -> update_model (Tick ts)));
         ("event", Js.Unsafe.inject (fun ev -> update_model (Event ev)));
         ( "view",
           Js.Unsafe.inject (fun () ->
               match !model with
               | Some m ->
                   Regl_common.encode_frame_pb (view m)
                   |> Regl_transport.uint8array_of_bytes |> Js.Unsafe.inject
               | None -> Js.Unsafe.inject Js.null) );
         ( "recvREGLCmdPb",
           Js.Unsafe.inject (fun recvcmd ->
               let payload =
                 Regl_transport.bytes_of_uint8array (Js.Unsafe.coerce recvcmd)
               in
               match decode_backend_event_pb payload with
               | Some msg -> update_model (REGLRecvMsg msg)
               | None -> ()) );
         ( "recvAudioMsgPb",
           Js.Unsafe.inject (fun recvmsg ->
               let payload =
                 Regl_transport.bytes_of_uint8array (Js.Unsafe.coerce recvmsg)
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
               | None -> ()) );
       |])
