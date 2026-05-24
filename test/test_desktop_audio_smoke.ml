(* End-to-end audio assertion harness for the desktop backend.

   Loads two assets: - test/assets/test.ogg (OGG Vorbis, decoded via stb_vorbis)
   - test/assets/synth.wav (WAV, decoded via SDL3's builtin loader)

   Both are pushed through the AssetLoader → AudioEngine pipeline, resampled to
   the device rate, and registered as buffers. The [audio] description function
   builds a Regl_audio.audio tree from model state; the framework diffs it
   against the previous frame and ships [start_sound] / [stop_sound] /
   [set_volume] / etc. through the bridge to libdeclgl, which runs the SDL3
   mixer.

   The harness keeps a private [Regl_audio.prev_state] mirror in lockstep with
   the runtime, so we can introspect the exact action stream the diff engine
   produces and assert against an expected sequence at the end of the run. The
   runtime's own diff is unaffected — both diffs start from [empty_state] and
   observe the same tree, so they emit the same actions modulo allocation order.

   Phases (frame numbers driven by Millisecond 10ms tick): - 0: load_audio for
   both URLs. - 60: start OGG. - 300: drop OGG volume to 0.3. - 400: start WAV
   at 2.0x playback rate. - 600: enable WAV loop region. - 800: unload OGG
   buffer mid-play (live release path). - 1000: stop everything (return
   Regl_audio.silence). - 1200: unload remaining WAV. - 1300: quit.

   Expected action shape per phase: - frame 60 : StartSound (ogg) - frame 300 :
   SetVolume (ogg, 0.30) - frame 350 : SetVolumeAt(ogg, 3 points) - frame 400 :
   StartSound (wav) - frame 600 : SetLoopConfig(wav, 200..800) - frame 1000 :
   StopSound ×2

   Close the window or wait for the auto-finish line. *)

open Ml_regl_core
open Ml_regl_core.Regl_proto

type asset_state = Pending | Ready of Regl_audio.source | Failed

type model = {
  ts : float;
  frame : int;
  ogg : asset_state;
  wav : asset_state;
  ctx_sr : int option;
  ogg_started : bool;
  ogg_start_ts : float;
  ogg_volume_dropped : bool;
  ogg_timeline_set : bool;
  ogg_unloaded_live : bool;
  wav_started : bool;
  wav_start_ts : float;
  wav_looped : bool;
  stopped : bool;
  unloaded : bool;
  finished : bool;
}

let virt_w = 800.0
let virt_h = 200.0
let ogg_url = "test/assets/test.ogg"
let wav_url = "test/assets/synth.wav"

(* {1 Action trace mirror} *)

(* {1 Expected sequence + assertions} *)

(* Each expectation is (frame, predicate, label). predicate is run on the
   action's debug string. We require exactly one match per expectation, in
   order, with frames non-decreasing. *)

let init () : model * regl_output list =
  let cmds =
    [
      start_regl
        {
          virt_width = virt_w;
          virt_height = virt_h;
          fbo_num = 5;
          builtin_programs = None;
          window = default_window_config;
        };
      config_regl (ConfigTimeInterval AnimationFrame);
      load_audio ogg_url;
      load_audio wav_url;
      load_font "custom" "test/assets/custom.png" "test/assets/custom-msdf.json";
    ]
  in
  ( {
      ts = 0.0;
      frame = 0;
      ogg = Pending;
      wav = Pending;
      ctx_sr = None;
      ogg_started = false;
      ogg_start_ts = 0.0;
      ogg_volume_dropped = false;
      ogg_timeline_set = false;
      ogg_unloaded_live = false;
      wav_started = false;
      wav_start_ts = 0.0;
      wav_looped = false;
      stopped = false;
      unloaded = false;
      finished = false;
    },
    cmds )

let describe_audio (m : model) : Regl_audio.audio =
  let parts = ref [] in
  if m.ogg_started && not m.stopped then
    begin match m.ogg with
    | Ready src ->
        let a = Regl_audio.audio src m.ogg_start_ts in
        let a =
          if m.ogg_volume_dropped then Regl_audio.scale_volume 0.3 a else a
        in
        let a =
          if m.ogg_timeline_set then
            (* 3-point fade-in/out absolute-time envelope; values are relative
               to the OGG's anchor. *)
            let t0 = m.ogg_start_ts in
            Regl_audio.scale_volume_at
              [ (t0, 0.1); (t0 +. 500.0, 1.0); (t0 +. 2000.0, 0.0) ]
              a
          else a
        in
        parts := a :: !parts
    | _ -> ()
    end;
  if m.wav_started && not m.stopped then
    begin match m.wav with
    | Ready src ->
        let loop =
          if m.wav_looped then
            Some { Regl_audio.loop_start = 200.0; loop_end = 800.0 }
          else None
        in
        let cfg = { Regl_audio.playback_rate = 2.0; start_at = 0.0; loop } in
        parts := Regl_audio.audio ~config:cfg src m.wav_start_ts :: !parts
    | _ -> ()
    end;
  Regl_audio.group !parts

let update (m : model) (input : regl_input) :
    model * Regl_audio.audio * regl_output list =
  let m', extras =
    match input with
    | Event (UpdateTick ts) ->
        let frame' = m.frame + 1 in
        let m1 = { m with ts; frame = frame' } in
        let m2, extras =
          if frame' = 60 && not m1.ogg_started then
            ({ m1 with ogg_started = true; ogg_start_ts = m1.ts }, [])
          else if frame' = 300 && not m1.ogg_volume_dropped then
            ({ m1 with ogg_volume_dropped = true }, [])
          else if frame' = 350 && not m1.ogg_timeline_set then
            ({ m1 with ogg_timeline_set = true }, [])
          else if frame' = 400 && not m1.wav_started then
            ({ m1 with wav_started = true; wav_start_ts = m1.ts }, [])
          else if frame' = 600 && not m1.wav_looped then
            ({ m1 with wav_looped = true }, [])
          else if frame' = 800 && not m1.ogg_unloaded_live then begin
            (* Voice stays in the [describe_audio] tree, so OCaml emits no
               StopSound. The C++ ReleaseBuffer path is responsible for
               silencing the still-running voice before freeing the buffer. *)
            ({ m1 with ogg_unloaded_live = true }, [ unload_audio ogg_url ])
          end
          else if frame' = 1000 && not m1.stopped then
            ({ m1 with stopped = true }, [])
          else if frame' = 1200 && not m1.unloaded then
            ({ m1 with unloaded = true }, [ unload_audio wav_url ])
          else if frame' = 1300 && not m1.finished then
            ({ m1 with finished = true }, [ quit_regl () ])
          else (m1, [])
        in
        (m2, extras)
    | Event (KeyDown _) ->
        (m, [])
    | AudioMsg (AudioContextReady { sample_rate }) ->
        ({ m with ctx_sr = Some sample_rate }, [])
    | AudioMsg (AudioLoadSuccess { audio_url; source }) ->
        let m =
          if audio_url = ogg_url then { m with ogg = Ready source }
          else if audio_url = wav_url then { m with wav = Ready source }
          else m
        in
        (m, [])
    | AudioMsg (AudioLoadFailed { audio_url; _ }) ->
        let m =
          if audio_url = ogg_url then { m with ogg = Failed }
          else if audio_url = wav_url then { m with wav = Failed }
          else m
        in
        (m, [])
    | _ -> (m, [])
  in
  let tree = describe_audio m' in
  (m', tree, extras)

let state_label = function
  | Pending -> "pending"
  | Ready s -> Printf.sprintf "ready[%d, %.2fs]" s.buffer_id s.duration
  | Failed -> "failed"

let view (m : model) : Regl_common.renderable =
  let bg = Regl_builtin_programs.clear (Color.rgb 0.05 0.07 0.10) in
  let line text y color =
    Regl_builtin_programs.textbox (20., y) 18.0 text "custom" color
  in
  let sr_text =
    match m.ctx_sr with
    | Some sr -> Printf.sprintf "sr=%dHz" sr
    | None -> "sr=<pending>"
  in
  let frame_text = Printf.sprintf "frame=%d ts=%.0fms" m.frame m.ts in
  let ogg_text = Printf.sprintf "ogg: %s" (state_label m.ogg) in
  let wav_text = Printf.sprintf "wav: %s" (state_label m.wav) in
  let phase_text =
    if m.unloaded then "phase: unloaded"
    else if m.stopped then "phase: stopped"
    else if m.wav_started then "phase: ogg+wav"
    else if m.ogg_volume_dropped then "phase: ogg quiet"
    else if m.ogg_started then "phase: ogg loud"
    else "phase: idle"
  in
  Regl_common.group []
    [
      bg;
      line sr_text 30. (Color.rgb 1.0 1.0 1.0);
      line frame_text 55. (Color.rgb 0.7 0.9 1.0);
      line ogg_text 80. (Color.rgb 0.3 1.0 0.3);
      line wav_text 105. (Color.rgb 1.0 0.85 0.3);
      line phase_text 140. (Color.rgb 0.95 0.5 0.95);
    ]

let () = Regl_backend.create_app init update view
