type source = { buffer_id : int; duration : float }
type load_error = FailedToDecode | NetworkError | UnknownError
type loop = { loop_start : float; loop_end : float }

type play_config = {
  loop : loop option;
  playback_rate : float;
  start_at : float;
}

let default_config = { loop = None; playback_rate = 1.0; start_at = 0.0 }

type effect_type =
  | ScaleVolume of float
  | ScaleVolumeAt of (float * float) list
  | Offset of float

type audio =
  | Group of audio list
  | Basic of { source : source; start_time : float; settings : play_config }
  | Effect of { effect_type : effect_type; audio : audio }

let audio ?(config = default_config) source start_time =
  Basic { source; start_time; settings = config }

let silence = Group []
let group xs = Group xs

let scale_volume scale_by a =
  Effect { effect_type = ScaleVolume (max 0.0 scale_by); audio = a }

let scale_volume_at points a =
  let points =
    points
    |> List.map (fun (t, v) -> (t, max 0.0 v))
    |> List.sort (fun (a, _) (b, _) -> compare a b)
  in
  let points = if points = [] then [ (0.0, 1.0) ] else points in
  Effect { effect_type = ScaleVolumeAt points; audio = a }

let offset_by d a = Effect { effect_type = Offset d; audio = a }
let length s = s.duration

(* {1 Flatten} *)

type flat = {
  source : source;
  start_time : float;
  start_at : float;
  offset : float;
  volume : float;
  volume_timelines : (float * float) list list;
  loop : loop option;
  playback_rate : float;
}

let rec flatten = function
  | Group xs -> List.concat_map flatten xs
  | Basic { source; start_time; settings } ->
      [
        {
          source;
          start_time;
          start_at = settings.start_at;
          offset = 0.0;
          volume = 1.0;
          volume_timelines = [];
          loop = settings.loop;
          playback_rate = settings.playback_rate;
        };
      ]
  | Effect { effect_type = ScaleVolume s; audio } ->
      List.map (fun f -> { f with volume = s *. f.volume }) (flatten audio)
  | Effect { effect_type = ScaleVolumeAt pts; audio } ->
      List.map
        (fun f -> { f with volume_timelines = pts :: f.volume_timelines })
        (flatten audio)
  | Effect { effect_type = Offset d; audio } ->
      List.map (fun f -> { f with offset = d +. f.offset }) (flatten audio)

(* effective absolute start time after [offset_by] is applied *)
let abs_start_time f = f.start_time +. f.offset

(* volume timelines shifted by the same offset *)
let shifted_volume_timelines f =
  List.map
    (fun tl -> List.map (fun (t, v) -> (t +. f.offset, v)) tl)
    f.volume_timelines

module Audio_pb = Transport_audio.Mlregl.Transport.Audio

type audio_action =
  | StartSound of int * flat
  | StopSound of int
  | SetVolume of int * float
  | SetLoopConfig of int * loop option
  | SetPlaybackRate of int * float
  | SetVolumeAt of int * flat

(* {1 Encoders} *)

let proto_volume_point (time, volume) =
  Audio_pb.VolumePoint.make ~time ~volume ()

let proto_volume_timeline timeline =
  Audio_pb.VolumeTimeline.make ~points:(List.map proto_volume_point timeline) ()

let proto_volume_timelines timelines = List.map proto_volume_timeline timelines

let proto_loop = function
  | Some { loop_start; loop_end } ->
      Some (Audio_pb.LoopConfig.make ~loop_start ~loop_end ())
  | None -> None

let proto_action = function
  | StartSound (node_group_id, f) ->
      Audio_pb.AudioAction.make
        ~kind:
          (`Start_sound
             (Audio_pb.StartSound.make ~node_group_id
                ~buffer_id:f.source.buffer_id ~start_time:(abs_start_time f)
                ~start_at:f.start_at ~volume:f.volume
                ~volume_timelines:
                  (proto_volume_timelines (shifted_volume_timelines f))
                ?loop:(proto_loop f.loop) ~playback_rate:f.playback_rate ()))
        ()
  | StopSound node_group_id ->
      Audio_pb.AudioAction.make
        ~kind:(`Stop_sound (Audio_pb.StopSound.make ~node_group_id ()))
        ()
  | SetVolume (node_group_id, volume) ->
      Audio_pb.AudioAction.make
        ~kind:(`Set_volume (Audio_pb.SetVolume.make ~node_group_id ~volume ()))
        ()
  | SetLoopConfig (node_group_id, loop) ->
      Audio_pb.AudioAction.make
        ~kind:
          (`Set_loop_config
             (Audio_pb.SetLoopConfig.make ~node_group_id ?loop:(proto_loop loop)
                ()))
        ()
  | SetPlaybackRate (node_group_id, playback_rate) ->
      Audio_pb.AudioAction.make
        ~kind:
          (`Set_playback_rate
             (Audio_pb.SetPlaybackRate.make ~node_group_id ~playback_rate ()))
        ()
  | SetVolumeAt (node_group_id, f) ->
      Audio_pb.AudioAction.make
        ~kind:
          (`Set_volume_at
             (Audio_pb.SetVolumeAt.make ~node_group_id
                ~volume_at:(proto_volume_timelines (shifted_volume_timelines f))
                ()))
        ()

let encode_command_batch_pb actions =
  let batch =
    Audio_pb.AudioCommandBatch.make ~actions:(List.map proto_action actions) ()
  in
  Audio_pb.AudioCommandBatch.to_proto batch
  |> Ocaml_protoc_plugin.Writer.contents |> Bytes.of_string

(* {1 Diff} *)

(* The previous state maps [node_group_id] -> previous flat audio, and tracks
   the next id to allocate. Stored in association lists for simplicity; counts
   are typically small. *)

type prev_state = { entries : (int * flat) list; next_id : int }

let empty_state = { entries = []; next_id = 0 }

(* Equality of two flat audio entries except for the "incrementally updatable"
   fields (volume, loop, playback_rate, volume timelines). Two entries match if
   they share buffer + effective start time + start_at. *)
let same_voice (a : flat) (b : flat) =
  a.source.buffer_id = b.source.buffer_id
  && abs_start_time a = abs_start_time b
  && a.start_at = b.start_at

let flat_equal (a : flat) (b : flat) =
  same_voice a b && a.volume = b.volume && a.loop = b.loop
  && a.playback_rate = b.playback_rate
  && shifted_volume_timelines a = shifted_volume_timelines b

(* Pop the first element of [xs] that satisfies [p]; return Some(elem, rest) or
   None. *)
let pop_first p xs =
  let rec loop acc = function
    | [] -> None
    | x :: rest when p x -> Some (x, List.rev_append acc rest)
    | x :: rest -> loop (x :: acc) rest
  in
  loop [] xs

let diff_actions (prev : prev_state) (new_audio : audio) :
    prev_state * audio_action list =
  let new_flats = flatten new_audio in
  (* For each previous entry, try to find a still-valid voice in [new_flats].
     Three outcomes: - exact match: reuse the entry, no message - same voice but
     different settings: reuse entry + emit incremental updates (set volume /
     loop / rate / volume timeline) - no matching voice: emit stopSound, drop
     entry *)
  let rec walk prev_entries new_flats msgs kept =
    match prev_entries with
    | [] -> (kept, new_flats, msgs)
    | (id, old_flat) :: rest -> (
        match pop_first (flat_equal old_flat) new_flats with
        | Some (_, leftover) ->
            (* perfect match *)
            walk rest leftover msgs ((id, old_flat) :: kept)
        | None -> (
            match pop_first (same_voice old_flat) new_flats with
            | Some (new_flat, leftover) ->
                let m = ref msgs in
                if old_flat.volume <> new_flat.volume then
                  m := SetVolume (id, new_flat.volume) :: !m;
                if old_flat.loop <> new_flat.loop then
                  m := SetLoopConfig (id, new_flat.loop) :: !m;
                if old_flat.playback_rate <> new_flat.playback_rate then
                  m := SetPlaybackRate (id, new_flat.playback_rate) :: !m;
                if
                  shifted_volume_timelines old_flat
                  <> shifted_volume_timelines new_flat
                then m := SetVolumeAt (id, new_flat) :: !m;
                walk rest leftover !m ((id, new_flat) :: kept)
            | None -> walk rest new_flats (StopSound id :: msgs) kept))
  in
  let kept, leftovers, msgs = walk prev.entries new_flats [] [] in
  (* Anything left in [leftovers] is a brand-new voice. *)
  let next_id = ref prev.next_id in
  let entries = ref kept in
  let msgs = ref msgs in
  List.iter
    (fun f ->
      let id = !next_id in
      incr next_id;
      entries := (id, f) :: !entries;
      msgs := StartSound (id, f) :: !msgs)
    leftovers;
  ({ entries = !entries; next_id = !next_id }, List.rev !msgs)

(* {1 Load + recv} *)

type recv_msg =
  | LoadSuccess of { audio_url : string; source : source }
  | LoadFailed of { audio_url : string; error : load_error }
  | ContextReady of { sample_rate : int }

let decode_pb_load_error = function
  | Audio_pb.AudioLoadError.AUDIO_LOAD_ERROR_FAILED_TO_DECODE -> FailedToDecode
  | Audio_pb.AudioLoadError.AUDIO_LOAD_ERROR_NETWORK -> NetworkError
  | Audio_pb.AudioLoadError.AUDIO_LOAD_ERROR_UNKNOWN -> UnknownError

let decode_recv_msg_pb payload =
  try
    let reader = Ocaml_protoc_plugin.Reader.create (Bytes.to_string payload) in
    let ev = Audio_pb.AudioBackendEvent.from_proto_exn reader in
    match ev with
    | `Audio_context_ready sample_rate -> Some (ContextReady { sample_rate })
    | `Audio_load_success msg ->
        Some
          (LoadSuccess
             {
               audio_url = msg.audio_url;
               source = { buffer_id = msg.buffer_id; duration = msg.duration };
             })
    | `Audio_load_failed msg ->
        Some
          (LoadFailed
             {
               audio_url = msg.audio_url;
               error = decode_pb_load_error msg.error;
             })
    | `not_set -> None
  with _ -> None
