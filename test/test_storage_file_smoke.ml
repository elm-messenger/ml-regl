(* Cross-backend smoke test for SaveValue / ReadValue / LoadFile.

   JS uses localStorage for SaveValue / ReadValue and fetch for LoadFile.
   Desktop performs storage and file I/O on AssetLoader's worker thread. This
   test verifies the OCaml-visible command/event contract without printing to
   stdout/stderr:

   - SaveValue is fire-and-forget. - ReadValue returns the value saved earlier
   in the same startup batch. - ReadValue for a never-saved key returns
   REGLValueReadMissing. - LoadFile returns text bytes for an existing file. -
   LoadFile returns REGLFileLoadFailed for a missing file.

   The window exits automatically once all expectations are met, or exits
   non-zero after a timeout if any expectation fails or is missing. *)

open Ml_regl_core
open Ml_regl_core.Regl_proto

let virt_w = 720.0
let virt_h = 260.0
let quit_after_ms = 1_000.0
let storage_key = "ml-regl-storage-smoke-key"
let storage_value = "stored value from test_storage_file_smoke"
let missing_key = "ml-regl-storage-smoke-missing-key-never-written"
let existing_file = "test/assets/Consolas.json"
let missing_file = "test/assets/does-not-exist-storage-smoke.txt"
let had_failure = ref false

type model = {
  ts : float;
  frame : int;
  value_ok : bool;
  missing_value_ok : bool;
  file_ok : bool;
  missing_file_ok : bool;
  failures : string list;
  quitting : bool;
}

let initial_model =
  {
    ts = 0.0;
    frame = 0;
    value_ok = false;
    missing_value_ok = false;
    file_ok = false;
    missing_file_ok = false;
    failures = [];
    quitting = false;
  }

let fail m msg =
  had_failure := true;
  { m with failures = msg :: m.failures }

let all_done m =
  m.value_ok && m.missing_value_ok && m.file_ok && m.missing_file_ok
  && m.failures = []

let init () : model * regl_output list =
  ( initial_model,
    [
      start_regl
        {
          virt_width = virt_w;
          virt_height = virt_h;
          fbo_num = 2;
          builtin_programs = None;
          window = default_window_config;
          app_name = None;
        };
      config_regl (ConfigTimeInterval AnimationFrame);
      load_font "consolas" "test/assets/Consolas.png"
        "test/assets/Consolas.json";
      save_value storage_key storage_value;
      read_value storage_key;
      read_value missing_key;
      load_file existing_file;
      load_file missing_file;
    ] )

let validate_file_data data = String.length data > 32 && data.[0] = '{'

let maybe_quit m =
  if (not m.quitting) && m.ts >= quit_after_ms then
    let m =
      if all_done m then m
      else
        fail m
          (Printf.sprintf
             "timeout value=%b missing_value=%b file=%b missing_file=%b"
             m.value_ok m.missing_value_ok m.file_ok m.missing_file_ok)
    in
    ({ m with quitting = true }, [ quit_regl () ])
  else (m, [])

let update (m : model) (input : regl_input) :
    model * Regl_audio.audio * regl_output list =
  let m =
    match input with
    | Event (UpdateTick ts) -> { m with ts; frame = m.frame + 1 }
    | REGLRecvMsg (REGLValueRead { key; value }) when key = storage_key ->
        if value = storage_value then { m with value_ok = true }
        else fail m (Printf.sprintf "bad value for %s" key)
    | REGLRecvMsg (REGLValueRead { key; value = _ }) ->
        fail m (Printf.sprintf "unexpected value_read for %s" key)
    | REGLRecvMsg (REGLValueReadMissing key) when key = missing_key ->
        { m with missing_value_ok = true }
    | REGLRecvMsg (REGLValueReadMissing key) ->
        fail m (Printf.sprintf "unexpected value_read_missing for %s" key)
    | REGLRecvMsg (REGLFileLoaded { path; data }) when path = existing_file ->
        if validate_file_data data then { m with file_ok = true }
        else fail m (Printf.sprintf "bad file data for %s" path)
    | REGLRecvMsg (REGLFileLoaded { path; data = _ }) ->
        fail m (Printf.sprintf "unexpected file_loaded for %s" path)
    | REGLRecvMsg (REGLFileLoadFailed { path; reason = _ })
      when path = missing_file ->
        { m with missing_file_ok = true }
    | REGLRecvMsg (REGLFileLoadFailed { path; reason = _ }) ->
        fail m (Printf.sprintf "unexpected file_load_failed for %s" path)
    | _ -> m
  in
  let m, cmds = maybe_quit m in
  (m, Regl_audio.silence, cmds)

let status_label name ok =
  Printf.sprintf "%s: %s" name (if ok then "ok" else "pending")

let text y s color =
  Regl_builtin_programs.textbox (20.0, y) 18.0 s "consolas" color

let view (m : model) : Regl_common.renderable =
  let ok = all_done m in
  let bg = if ok then Color.rgb 0.04 0.12 0.07 else Color.rgb 0.07 0.08 0.12 in
  let fg = Color.rgb 0.94 0.96 0.98 in
  let muted = Color.rgb 0.70 0.76 0.82 in
  let accent =
    if ok then Color.rgb 0.25 0.95 0.45 else Color.rgb 0.95 0.75 0.25
  in
  let failure_lines =
    m.failures |> List.rev
    |> List.mapi (fun i msg ->
        text (176.0 +. (float i *. 20.0)) msg (Color.rgb 1.0 0.45 0.35))
  in
  Regl_common.group []
    ([
       Regl_builtin_programs.clear bg;
       text 24.0 "storage/file backend smoke test" fg;
       text 56.0 (status_label "SaveValue + ReadValue" m.value_ok) accent;
       text 80.0 (status_label "ReadValue missing" m.missing_value_ok) accent;
       text 104.0 (status_label "LoadFile existing" m.file_ok) accent;
       text 128.0 (status_label "LoadFile missing" m.missing_file_ok) accent;
       text 154.0
         (Printf.sprintf "frame=%d t=%.1fs failures=%d" m.frame (m.ts /. 1000.0)
            (List.length m.failures))
         muted;
     ]
    @ failure_lines)

let () =
  Regl_backend.create_app init update view;
  if !had_failure then exit 1
