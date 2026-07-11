(* Portable core of ml_regl. See regl_proto.mli for documentation.

   This module is intentionally free of Js_of_ocaml so the same code can be
   linked into native backends (e.g. declgl-desktop). The JS-coupled host glue
   (window callbacks, MlApp export, DOM event handling) lives in [regl.ml]. *)

type time_interval = AnimationFrame | Millisecond of float
type window_config = { fullscreen : bool option; resizable : bool option }

let default_window_config = { fullscreen = None; resizable = None }

type regl_config =
  | ConfigTimeInterval of time_interval
  | ConfigWindow of window_config
  | ConfigMaxAssetsPerFrame of int

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
  window : window_config;
  (* App identifier forwarded to the backend in [StartRegl.app_name]. The
     desktop backend uses it to scope SDL_GetPrefPath for KV storage (per-app
     on-disk state) and reports it in policy diagnostics. Empty / [None] ->
     backend default ("declgl"). *)
  app_name : string option;
}

type texture = { name : string; width : int; height : int }

type regl_recv_msg =
  | REGLTextureLoaded of texture
  | REGLTextureLoadFail of { name : string; reason : string }
  | REGLFontLoaded of string
  | REGLFontLoadFail of { name : string; reason : string }
  | REGLProgramCreated of string
  | REGLProgramCreateFail of string
  | REGLValueRead of { key : string; value : string }
  | REGLValueReadMissing of string
  | REGLFileLoaded of { path : string; data : string }
  | REGLFileLoadFailed of { path : string; reason : string }

type audio_recv_msg =
  | AudioLoadSuccess of { audio_url : string; source : Regl_audio.source }
  | AudioLoadFailed of { audio_url : string; error : Regl_audio.load_error }
  | AudioContextReady of { sample_rate : int }

(* The [regl_input] variant lives in each host facade (it carries the host's
   native event type). The core only needs the message payload types declared
   above. *)

module Backend_pb = Transport_backend.Mlregl.Transport.Backend
module Common_pb = Transport_common.Mlregl.Transport.Common

type regl_output = Backend_pb.BackendCommand.t

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

let decode_backend_event_pb (payload : bytes) : regl_recv_msg option =
  try
    let reader =
      Ocaml_protoc_plugin.Reader.create (Bytes.unsafe_to_string payload)
    in
    match Backend_pb.BackendEvent.from_proto_exn reader with
    | `Texture_loaded { name; width; height } ->
        Some (REGLTextureLoaded { name; width; height })
    | `Texture_loadfail { name; reason } ->
        Some (REGLTextureLoadFail { name; reason })
    | `Font_loaded name -> Some (REGLFontLoaded name)
    | `Font_loadfail { name; reason } ->
        Some (REGLFontLoadFail { name; reason })
    | `Program_created name -> Some (REGLProgramCreated name)
    | `Program_createfail name -> Some (REGLProgramCreateFail name)
    | `Value_read { key; value } -> Some (REGLValueRead { key; value })
    | `Value_read_missing key -> Some (REGLValueReadMissing key)
    | `File_loaded { path; data } -> Some (REGLFileLoaded { path; data })
    | `File_load_failed { path; reason } ->
        Some (REGLFileLoadFailed { path; reason })
    | `not_set -> None
  with _ -> None

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
  let { fullscreen; resizable } = cfg.window in
  let window =
    match (fullscreen, resizable) with
    | None, None -> None
    | _ -> Some (Backend_pb.WindowConfig.make ?fullscreen ?resizable ())
  in
  let app_name = match cfg.app_name with Some s -> s | None -> "" in
  Backend_pb.BackendCommand.make
    ~kind:
      (match cfg.builtin_programs with
      | None ->
          `Start_regl
            (Backend_pb.StartRegl.make ~virt_width:cfg.virt_width
               ~virt_height:cfg.virt_height ~fbo_num:cfg.fbo_num ~app_name
               ?window ())
      | Some xs ->
          let bps = Common_pb.StringArray.make ~values:xs () in
          `Start_regl
            (Backend_pb.StartRegl.make ~virt_width:cfg.virt_width
               ~virt_height:cfg.virt_height ~fbo_num:cfg.fbo_num ~app_name
               ~builtin_programs:bps ?window ()))
    ()

let create_regl_program name program =
  Backend_pb.BackendCommand.make
    ~kind:(`Create_program (backend_create_program name program))
    ()

let config_regl cfg =
  let config =
    match cfg with
    | ConfigTimeInterval ti ->
        let ms =
          match ti with AnimationFrame -> -1.0 | Millisecond ms -> ms
        in
        `Interval_ms ms
    | ConfigWindow { fullscreen; resizable } ->
        `Window (Backend_pb.WindowConfig.make ?fullscreen ?resizable ())
    | ConfigMaxAssetsPerFrame max_items ->
        `Max_assets_per_frame (max 0 max_items)
  in
  Backend_pb.BackendCommand.make
    ~kind:(`Config_regl (Backend_pb.ReglConfig.make ~config ()))
    ()

let load_audio audio_url =
  Backend_pb.BackendCommand.make
    ~kind:(`Load_audio (Backend_pb.LoadAudio.make ~audio_url ()))
    ()

let unload_texture name =
  Backend_pb.BackendCommand.make
    ~kind:(`Unload_texture (Backend_pb.UnloadTexture.make ~name ()))
    ()

let unload_font name =
  Backend_pb.BackendCommand.make
    ~kind:(`Unload_font (Backend_pb.UnloadFont.make ~name ()))
    ()

let unload_audio audio_url =
  Backend_pb.BackendCommand.make
    ~kind:(`Unload_audio (Backend_pb.UnloadAudio.make ~audio_url ()))
    ()

let quit_regl () =
  Backend_pb.BackendCommand.make
    ~kind:(`Quit_regl (Backend_pb.QuitRegl.make ()))
    ()

let save_value key value =
  Backend_pb.BackendCommand.make
    ~kind:(`Save_value (Backend_pb.SaveValue.make ~key ~value ()))
    ()

let read_value key =
  Backend_pb.BackendCommand.make
    ~kind:(`Read_value (Backend_pb.ReadValue.make ~key ()))
    ()

let load_file path =
  Backend_pb.BackendCommand.make
    ~kind:(`Load_file (Backend_pb.LoadFile.make ~path ()))
    ()

type regl_event =
  | UpdateTick of float
  | MouseDown of { button : int; x : float; y : float }
  | MouseUp of { button : int; x : float; y : float }
  | MouseMove of { x : float; y : float }
  | KeyDown of string (* Key Code *)
  | KeyUp of string (* Key Code *)

type regl_input =
  | Event of regl_event
  | REGLRecvMsg of regl_recv_msg
  | AudioMsg of audio_recv_msg

let decode_event_pb (payload : bytes) : regl_event option =
  let reader =
    Ocaml_protoc_plugin.Reader.create (Bytes.unsafe_to_string payload)
  in
  match Backend_pb.Event.from_proto_exn reader with
  | `Update_tick t -> Some (UpdateTick t)
  | `Mouse_down { button; x; y } -> Some (MouseDown { button; x; y })
  | `Mouse_up { button; x; y } -> Some (MouseUp { button; x; y })
  | `Mouse_move { x; y } -> Some (MouseMove { x; y })
  | `Key_down key -> Some (KeyDown key)
  | `Key_up key -> Some (KeyUp key)
  | `not_set -> None
