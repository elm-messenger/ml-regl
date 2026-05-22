(* See regl_runtime.mli for documentation. *)

module type Host = sig
  val ship_backend_cmd : bytes -> unit
  val ship_audio_cmd : bytes -> unit
end

module Make (H : Host) = struct
  type 'model handle = {
    init : unit -> unit;
    event : bytes -> unit;
    view : unit -> bytes option;
    recv_regl_cmd_pb : bytes -> unit;
    recv_audio_msg_pb : bytes -> unit;
  }

  let create_app ~init ~update ~view =
    let model = ref None in
    let audio_state = ref Regl_audio.empty_state in

    let ship_cmds cmds =
      if cmds <> [] then
        H.ship_backend_cmd (Regl_proto.encode_backend_command_batch_pb cmds)
    in
    let ship_audio_actions actions =
      if actions <> [] then
        H.ship_audio_cmd (Regl_audio.encode_command_batch_pb actions)
    in

    let drive input =
      match !model with
      | Some m ->
          let m', audio_tree, outputs = update m input in
          model := Some m';
          ship_cmds outputs;
          let new_state, audio_actions =
            Regl_audio.diff_actions !audio_state audio_tree
          in
          audio_state := new_state;
          ship_audio_actions audio_actions
      | None -> ()
    in

    {
      init =
        (fun () ->
          let m, outputs = init () in
          model := Some m;
          ship_cmds outputs);
      event =
        (fun payload ->
          match Regl_proto.decode_event_pb payload with
          | Some ev -> drive (Regl_proto.Event ev)
          | None -> ());
      view =
        (fun () ->
          match !model with
          | Some m -> Some (Regl_common.encode_frame_pb (view m))
          | None -> None);
      recv_regl_cmd_pb =
        (fun payload ->
          match Regl_proto.decode_backend_event_pb payload with
          | Some msg -> drive (Regl_proto.REGLRecvMsg msg)
          | None -> ());
      recv_audio_msg_pb =
        (fun payload ->
          match Regl_audio.decode_recv_msg_pb payload with
          | Some (Regl_audio.LoadSuccess { audio_url; source }) ->
              drive
                (AudioMsg (Regl_proto.AudioLoadSuccess { audio_url; source }))
          | Some (Regl_audio.LoadFailed { audio_url; error }) ->
              drive (AudioMsg (Regl_proto.AudioLoadFailed { audio_url; error }))
          | Some (Regl_audio.ContextReady { sample_rate }) ->
              drive (AudioMsg (Regl_proto.AudioContextReady { sample_rate }))
          | None -> ());
    }
end
