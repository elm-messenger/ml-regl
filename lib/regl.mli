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

val load_texture : string -> string -> texture_options option -> regl_output
val load_font : string -> string -> string -> regl_output
val start_regl : regl_start_config -> regl_output
val create_regl_program : string -> Regl_program.regl_program -> regl_output
val config_regl : regl_config -> regl_output
val load_audio : string -> regl_output

val create_app :
  (Dom_html.canvasElement Js.t option -> 'a * regl_output list) ->
  (Dom_html.canvasElement Js.t option ->
  'a ->
  regl_input ->
  'a * Regl_audio.audio * regl_output list) ->
  ('a -> Regl_common.renderable) ->
  unit
