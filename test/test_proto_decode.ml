(* Storage read replies are decoded as [ValueRead] events; other backend replies
   stay [REGLRecvMsg]. *)

open Ml_regl_core
module B = Regl_proto.Backend_pb

let encode kind =
  B.BackendEvent.make ~kind ()
  |> B.BackendEvent.to_proto |> Ocaml_protoc_plugin.Writer.contents
  |> Bytes.of_string

let decode kind = Regl_proto.decode_backend_event_pb (encode kind)

let () =
  assert (
    decode (`Value_read (B.ValueRead.make ~key:"best" ~value:"42" ()))
    = Some (Event (ValueRead { key = "best"; value = Some "42" })));
  assert (
    decode (`Value_read_missing (B.ValueReadMissing.make ~key:"none" ()))
    = Some (Event (ValueRead { key = "none"; value = None })));
  assert (
    decode (`File_loaded (B.FileLoaded.make ~path:"a.txt" ~data:"text" ()))
    = Some (REGLRecvMsg (REGLFileLoaded { path = "a.txt"; data = "text" })));
  assert (decode `not_set = None)
