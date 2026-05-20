(** Declarative audio for ml-regl.

    Inspired by [elm-audio]. The user describes which sounds *should* be playing
    as a function of model state; the diff against the previous description is
    turned into start / stop / setVolume / etc. messages. *)

(** A loaded audio buffer. Returned in [AudioLoadSuccess]. *)
type source = { buffer_id : int; duration : float }
(** [duration] is in seconds. *)

type load_error = FailedToDecode | NetworkError | UnknownError

type loop = { loop_start : float; loop_end : float }
(** Looping config. Times are in milliseconds, relative to the start of the
    buffer. The audio loops from [loop_end] back to [loop_start]. *)

type play_config = {
  loop : loop option;
  playback_rate : float;
  start_at : float;  (** ms into the buffer to begin playing from *)
}
(** Optional settings when playing audio. *)

val default_config : play_config

type audio
(** Audio description tree. Abstract — build with the combinators below. *)

val audio : ?config:play_config -> source -> float -> audio
(** [audio ?config source start_time] plays [source] starting at the absolute
    time [start_time] (milliseconds, in the same scale as [Tick]). *)

val silence : audio
(** No audio at all. *)

val group : audio list -> audio
(** Combine multiple audio descriptions. *)

val scale_volume : float -> audio -> audio
(** Multiply the volume of an audio description. Clamped at 0. *)

val scale_volume_at : (float * float) list -> audio -> audio
(** Volume timeline: list of (time, volume) points. The volume is linearly
    interpolated between consecutive points. Times are absolute milliseconds. *)

val offset_by : float -> audio -> audio
(** Shift every audio start time by the given duration (milliseconds). *)

val length : source -> float
(** Duration of a loaded source, in seconds. *)

(** {1 Internal — used by [Regl.create_app]} *)

type prev_state
(** Opaque diff state kept across frames. *)

val empty_state : prev_state

type audio_action
(** Opaque diff action used by the transport layer. *)

val diff_actions : prev_state -> audio -> prev_state * audio_action list
(** [diff_actions prev new_audio] produces an updated state and typed audio
    actions describing how the backend should be updated. *)

val encode_command_batch_pb : audio_action list -> bytes
(** Encode an audio action batch into protobuf wire format. *)

(** Decode an incoming message from the JS audio runtime. Returns [None] for
    messages that aren't audio-related or are malformed. *)
type recv_msg =
  | LoadSuccess of { audio_url : string; source : source }
  | LoadFailed of { audio_url : string; error : load_error }
  | ContextReady of { sample_rate : int }

val decode_recv_msg_pb : bytes -> recv_msg option
(** Decode a protobuf event from the JS audio runtime. Returns [None] on
    malformed payloads. *)
