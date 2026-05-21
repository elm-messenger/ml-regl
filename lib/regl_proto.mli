(** Portable core of ml_regl: protobuf encoding helpers and pure types shared by
    all backends (JS, native, ...). No [Js_of_ocaml] dependency.

    The [regl_input] type is parameterized by the host's native event type so
    this core can be reused in environments that have no DOM. *)

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
  | REGLTextureLoadFail of string
  | REGLFontLoaded of string
  | REGLFontLoadFail of string
  | REGLProgramCreated of string
  | REGLProgramCreateFail of string

type audio_recv_msg =
  | AudioLoadSuccess of { audio_url : string; source : Regl_audio.source }
  | AudioLoadFailed of { audio_url : string; error : Regl_audio.load_error }
  | AudioContextReady of { sample_rate : int }

(** Note: the [regl_input] variant (which carries the host's native event type)
    lives in each host facade ([Regl] for the JS build, the native driver for
    declgl-desktop). The core only needs the message payload types
    ([regl_recv_msg], [audio_recv_msg]) above. *)

module Backend_pb : module type of Transport_backend.Mlregl.Transport.Backend
module Common_pb : module type of Transport_common.Mlregl.Transport.Common

type regl_output = Backend_pb.BackendCommand.t

val backend_mag : texture_mag_option -> Backend_pb.TextureMagOption.t
val backend_min : texture_min_option -> Backend_pb.TextureMinOption.t

val backend_texture_options :
  texture_options option -> Backend_pb.TextureOptions.t option

val backend_create_program :
  string -> Regl_program.regl_program -> Backend_pb.CreateProgram.t

val encode_backend_command_batch_pb : Backend_pb.BackendCommand.t list -> bytes
(** Encode a list of [BackendCommand.t] into protobuf bytes (a
    [BackendCommandBatch] message). *)

val decode_backend_event_pb : bytes -> regl_recv_msg option
(** Decode a [BackendEvent] protobuf payload coming from the host. *)


(** Smart constructors for [BackendCommand]s. *)

val load_texture : string -> string -> texture_options option -> regl_output
val load_font : string -> string -> string -> regl_output
val start_regl : regl_start_config -> regl_output
val create_regl_program : string -> Regl_program.regl_program -> regl_output
val config_regl : regl_config -> regl_output
val load_audio : string -> regl_output

(** Symmetric inverses of the [load_*] commands. The backend frees the
    underlying GPU resource (where applicable) before dropping the CPU-
    side metadata; in-flight loads matching the same identity are
    cancelled where possible. Unloading something that was never loaded
    is a silent no-op. Identity keys mirror the corresponding [load_*]:

    - [unload_texture]: by [name] (matches [load_texture]'s [name]).
    - [unload_font]: by [name] (matches [load_font]'s [name]). The
      atlas texture is freed too.
    - [unload_audio]: by [audio_url] (matches [load_audio]'s sole arg). *)

val unload_texture : string -> regl_output
val unload_font : string -> regl_output
val unload_audio : string -> regl_output

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

val decode_event_pb : bytes -> regl_event option
