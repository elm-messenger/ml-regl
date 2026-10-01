(* Window config encoding: [title] survives the wire on both entry points
   (StartRegl.window and ConfigRegl/ConfigWindow), and an all-[None] start
   config still omits the WindowConfig message. *)

open Ml_regl_core
open Regl_proto

let roundtrip (cmd : regl_output) : regl_output =
  let bytes = encode_backend_command_batch_pb [ cmd ] in
  match
    Backend_pb.BackendCommandBatch.from_proto
      (Ocaml_protoc_plugin.Reader.create (Bytes.to_string bytes))
  with
  | Ok [ cmd ] -> cmd
  | _ -> assert false

let start window =
  start_regl
    {
      virt_width = 640.0;
      virt_height = 480.0;
      fbo_num = 1;
      builtin_programs = None;
      window;
      app_name = None;
    }

let start_window cmd =
  match roundtrip cmd with `Start_regl s -> s.window | _ -> assert false

let config_window cmd =
  match roundtrip cmd with `Config_regl (`Window w) -> w | _ -> assert false

let () =
  assert (start_window (start default_window_config) = None);
  (match
     start_window (start { default_window_config with title = Some "Game" })
   with
  | Some w ->
      assert (w.title = Some "Game");
      assert (w.fullscreen = None);
      assert (w.resizable = None)
  | None -> assert false);
  let w =
    config_window
      (config_regl
         (ConfigWindow
            { fullscreen = None; resizable = Some true; title = Some "" }))
  in
  assert (w.title = Some "");
  assert (w.resizable = Some true);
  let w =
    config_window
      (config_regl
         (ConfigWindow { default_window_config with fullscreen = Some true }))
  in
  assert (w.title = None)
