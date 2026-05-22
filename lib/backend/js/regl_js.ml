(* ml_regl JS facade. The portable core is in [Regl_proto] and the host-agnostic
   runtime is in [Regl_runtime]; here we add the [Js_of_ocaml]-flavored host
   (DOM events, MlREGL global, MlApp export) and re-export everything the
   existing tests expect. *)

open Js_of_ocaml

(* include Regl_proto *)
open Ml_regl_core
open Ml_regl_core.Regl_proto

let uint8array_of_bytes bytes = Typed_array.Bytes.to_uint8Array bytes
let bytes_of_uint8array arr = Typed_array.Bytes.of_uint8Array arr

let execCmdPb commands =
  let mlregl = Js.Unsafe.global##.MlREGL in
  let payload = encode_backend_command_batch_pb commands in
  let bytes = uint8array_of_bytes payload in
  Js.Unsafe.fun_call
    (Js.Unsafe.get mlregl "execCmdPb")
    [| Js.Unsafe.inject bytes |]

let execAudioCmdPb actions =
  let mlregl = Js.Unsafe.global##.MlREGL in
  let payload = Regl_audio.encode_command_batch_pb actions in
  let bytes = uint8array_of_bytes payload in
  Js.Unsafe.fun_call
    (Js.Unsafe.get mlregl "execAudioCmdPb")
    [| Js.Unsafe.inject bytes |]

(* The JS host: event = DOM event, canvas = canvas element, byte-shippers talk
   to the global MlREGL object via Regl_transport. *)
module JsHost = struct
  let send name payload =
    let mlregl = Js.Unsafe.global##.MlREGL in
    let bytes = uint8array_of_bytes payload in
    Js.Unsafe.fun_call (Js.Unsafe.get mlregl name) [| Js.Unsafe.inject bytes |]

  let ship_backend_cmd payload = send "execCmdPb" payload
  let ship_audio_cmd payload = send "execAudioCmdPb" payload
end

module JsRuntime = Regl_runtime.Make (JsHost)

let create_app (init : unit -> 'a * regl_output list)
    (update : 'a -> regl_input -> 'a * Regl_audio.audio * regl_output list)
    (view : 'a -> Regl_common.renderable) =
  let h = JsRuntime.create_app ~init ~update ~view in
  Js.export "MlApp"
    (Js.Unsafe.obj
       [|
         ("init", Js.Unsafe.inject (fun _ -> h.init ()));
         ( "event",
           Js.Unsafe.inject (fun ev ->
               h.event (bytes_of_uint8array (Js.Unsafe.coerce ev))) );
         ( "view",
           Js.Unsafe.inject (fun () ->
               match h.view () with
               | Some payload ->
                   payload |> uint8array_of_bytes |> Js.Unsafe.inject
               | None -> Js.Unsafe.inject Js.null) );
         ( "recvREGLCmdPb",
           Js.Unsafe.inject (fun recvcmd ->
               h.recv_regl_cmd_pb
                 (bytes_of_uint8array (Js.Unsafe.coerce recvcmd))) );
         ( "recvAudioMsgPb",
           Js.Unsafe.inject (fun recvmsg ->
               h.recv_audio_msg_pb
                 (bytes_of_uint8array (Js.Unsafe.coerce recvmsg))) );
       |])
