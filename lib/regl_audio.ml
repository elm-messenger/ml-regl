open Js_of_ocaml

type source = { buffer_id : int; duration : float }
type load_error = FailedToDecode | NetworkError | UnknownError
type loop = { loop_start : float; loop_end : float }

type play_config = {
  loop : loop option;
  playback_rate : float;
  start_at : float;
}

let default_config =
  { loop = None; playback_rate = 1.0; start_at = 0.0 }

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
  List.map (fun tl -> List.map (fun (t, v) -> (t +. f.offset, v)) tl)
    f.volume_timelines

(* {1 JS encoders} *)

let js_str s = Js.Unsafe.inject (Js.string s)
let js_num f = Js.Unsafe.inject (Js.number_of_float f)
let js_int i = Js.Unsafe.inject (Js.number_of_float (float_of_int i))

let encode_loop = function
  | Some { loop_start; loop_end } ->
      Js.Unsafe.inject
        (Js.Unsafe.obj
           [|
             ("loopStart", js_num loop_start);
             ("loopEnd", js_num loop_end);
           |])
  | None -> Js.Unsafe.inject Js.null

let encode_volume_timeline tl =
  let arr =
    Array.of_list
      (List.map
         (fun (t, v) ->
           Js.Unsafe.obj [| ("time", js_num t); ("volume", js_num v) |])
         tl)
  in
  Js.array arr

let encode_volume_timelines tls =
  let arr = Array.of_list (List.map encode_volume_timeline tls) in
  Js.array arr

let encode_start node_group_id (f : flat) : Js.Unsafe.any =
  Js.Unsafe.obj
    [|
      ("action", js_str "startSound");
      ("nodeGroupId", js_int node_group_id);
      ("bufferId", js_int f.source.buffer_id);
      ("startTime", js_num (abs_start_time f));
      ("startAt", js_num f.start_at);
      ("volume", js_num f.volume);
      ( "volumeTimelines",
        Js.Unsafe.inject (encode_volume_timelines (shifted_volume_timelines f))
      );
      ("loop", encode_loop f.loop);
      ("playbackRate", js_num f.playback_rate);
    |]

let encode_stop node_group_id : Js.Unsafe.any =
  Js.Unsafe.obj
    [|
      ("action", js_str "stopSound"); ("nodeGroupId", js_int node_group_id);
    |]

let encode_set_volume node_group_id volume : Js.Unsafe.any =
  Js.Unsafe.obj
    [|
      ("action", js_str "setVolume");
      ("nodeGroupId", js_int node_group_id);
      ("volume", js_num volume);
    |]

let encode_set_loop node_group_id loop : Js.Unsafe.any =
  Js.Unsafe.obj
    [|
      ("action", js_str "setLoopConfig");
      ("nodeGroupId", js_int node_group_id);
      ("loop", encode_loop loop);
    |]

let encode_set_playback_rate node_group_id rate : Js.Unsafe.any =
  Js.Unsafe.obj
    [|
      ("action", js_str "setPlaybackRate");
      ("nodeGroupId", js_int node_group_id);
      ("playbackRate", js_num rate);
    |]

let encode_set_volume_at node_group_id (f : flat) : Js.Unsafe.any =
  Js.Unsafe.obj
    [|
      ("action", js_str "setVolumeAt");
      ("nodeGroupId", js_int node_group_id);
      ( "volumeAt",
        Js.Unsafe.inject (encode_volume_timelines (shifted_volume_timelines f))
      );
    |]

(* {1 Diff} *)

(* The previous state maps [node_group_id] -> previous flat audio, and tracks
   the next id to allocate. Stored in association lists for simplicity; counts
   are typically small. *)

type prev_state = { entries : (int * flat) list; next_id : int }

let empty_state = { entries = []; next_id = 0 }

(* Equality of two flat audio entries except for the "incrementally
   updatable" fields (volume, loop, playback_rate, volume timelines). Two
   entries match if they share buffer + effective start time + start_at. *)
let same_voice (a : flat) (b : flat) =
  a.source.buffer_id = b.source.buffer_id
  && abs_start_time a = abs_start_time b
  && a.start_at = b.start_at

let flat_equal (a : flat) (b : flat) =
  same_voice a b
  && a.volume = b.volume
  && a.loop = b.loop
  && a.playback_rate = b.playback_rate
  && shifted_volume_timelines a = shifted_volume_timelines b

(* Pop the first element of [xs] that satisfies [p]; return Some(elem, rest)
   or None. *)
let pop_first p xs =
  let rec loop acc = function
    | [] -> None
    | x :: rest when p x -> Some (x, List.rev_append acc rest)
    | x :: rest -> loop (x :: acc) rest
  in
  loop [] xs

let diff (prev : prev_state) (new_audio : audio) :
    prev_state * Js.Unsafe.any list =
  let new_flats = flatten new_audio in
  (* For each previous entry, try to find a still-valid voice in [new_flats].
     Three outcomes:
     - exact match: reuse the entry, no message
     - same voice but different settings: reuse entry + emit incremental
       updates (set volume / loop / rate / volume timeline)
     - no matching voice: emit stopSound, drop entry *)
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
                  m := encode_set_volume id new_flat.volume :: !m;
                if old_flat.loop <> new_flat.loop then
                  m := encode_set_loop id new_flat.loop :: !m;
                if old_flat.playback_rate <> new_flat.playback_rate then
                  m := encode_set_playback_rate id new_flat.playback_rate :: !m;
                if
                  shifted_volume_timelines old_flat
                  <> shifted_volume_timelines new_flat
                then m := encode_set_volume_at id new_flat :: !m;
                walk rest leftover !m ((id, new_flat) :: kept)
            | None ->
                walk rest new_flats (encode_stop id :: msgs) kept))
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
      msgs := encode_start id f :: !msgs)
    leftovers;
  ({ entries = !entries; next_id = !next_id }, List.rev !msgs)

(* {1 Load + recv} *)

let encode_load_request request_id url : Js.Unsafe.any =
  Js.Unsafe.obj
    [|
      ("requestId", js_int request_id);
      ("audioUrl", js_str url);
    |]

type recv_msg =
  | LoadSuccess of { request_id : int; source : source }
  | LoadFailed of { request_id : int; error : load_error }
  | ContextReady of { sample_rate : int }

let decode_load_error = function
  | "NetworkError" -> NetworkError
  | "FailedToDecode" -> FailedToDecode
  | _ -> UnknownError

let decode_recv_msg v =
  let get_string path =
    try Some (Js.to_string (Js.Unsafe.get v (Js.string path))) with _ -> None
  in
  let get_int path =
    try
      Some
        (int_of_float (Js.float_of_number (Js.Unsafe.get v (Js.string path))))
    with _ -> None
  in
  let get_float path =
    try Some (Js.float_of_number (Js.Unsafe.get v (Js.string path)))
    with _ -> None
  in
  match get_string "_c" with
  | Some "audioLoadSuccess" -> (
      match
        ( get_int "requestId",
          get_int "bufferId",
          get_float "duration" )
      with
      | Some request_id, Some buffer_id, Some duration ->
          Some
            (LoadSuccess
               { request_id; source = { buffer_id; duration } })
      | _ -> None)
  | Some "audioLoadFailed" -> (
      match (get_int "requestId", get_string "error") with
      | Some request_id, Some err ->
          Some (LoadFailed { request_id; error = decode_load_error err })
      | _ -> None)
  | Some "audioContextReady" -> (
      match get_int "sampleRate" with
      | Some sample_rate -> Some (ContextReady { sample_rate })
      | None -> None)
  | _ -> None
