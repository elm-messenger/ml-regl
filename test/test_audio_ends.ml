(* [Regl_audio.ends_at]: offsets, start positions, rates, loops, groups. *)

open Ml_regl_core
open Regl_audio

let src = { buffer_id = 1; duration = 10.0 }

let config ?(rate = 1.0) ?(start_at = 0.0) ?loop () =
  { playback_rate = rate; start_at; loop }

let () =
  assert (ends_at (audio src 0.0) = Some 10_000.0);
  assert (ends_at (audio src 500.0) = Some 10_500.0);
  assert (ends_at (audio ~config:(config ~rate:2.0 ()) src 0.0) = Some 5_000.0);
  assert (ends_at (audio ~config:(config ~rate:0.5 ()) src 0.0) = Some 20_000.0);
  assert (
    ends_at (audio ~config:(config ~start_at:4_000.0 ()) src 0.0) = Some 6_000.0);
  assert (ends_at (offset_by 3_000.0 (audio src 0.0)) = Some 13_000.0);
  assert (ends_at (scale_volume 0.5 (audio src 0.0)) = Some 10_000.0);
  assert (
    ends_at (group [ audio src 0.0; offset_by 1_000.0 (audio src 0.0) ])
    = Some 11_000.0);
  let loop = { loop_start = 0.0; loop_end = 1_000.0 } in
  assert (ends_at (audio ~config:(config ~loop ()) src 0.0) = None);
  assert (
    ends_at (group [ audio src 0.0; audio ~config:(config ~loop ()) src 0.0 ])
    = None);
  assert (ends_at (audio ~config:(config ~rate:0.0 ()) src 0.0) = None);
  assert (ends_at silence = Some neg_infinity)
