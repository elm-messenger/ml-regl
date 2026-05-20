(* Native OCaml capture tool for the ml-regl protobuf wire protocol.

   This program builds a small but representative ml-regl scene, drives it
   through [Regl_runtime.Make] with a "file dump" host, and writes a binary
   capture file containing every BackendCommandBatch / AudioCommandBatch /
   Renderable that the scene produced. The C++ replay tool in
   [declgl-desktop/tools/replay/] reads this file and pushes each record through
   libdeclgl's C ABI to verify that the wire format is bit-faithful between the
   JS-verified OCaml frontend and the new native backend.

   File format (little-endian): magic "DGLCAP01" 8 bytes u32 record_count record
   := u8 kind (1=BE_CMD, 2=AU_CMD, 3=VIEW) u64 ts_ms u32 len u8[] payload [len
   bytes] *)
open Ml_regl_core

let magic = "DGLCAP01"
let kind_be_cmd = 1
let kind_au_cmd = 2
let kind_view = 3

(* Tiny LE writer over an out_channel. We pre-stage records into a buffer so the
   final file gets a correct record_count header. *)

module BE = struct
  let u8 buf v = Buffer.add_char buf (Char.chr (v land 0xff))

  let u32 buf v =
    u8 buf v;
    u8 buf (v lsr 8);
    u8 buf (v lsr 16);
    u8 buf (v lsr 24)

  let u64 buf v =
    let lo = Int64.to_int (Int64.logand v 0xffffffffL) in
    let hi = Int64.to_int (Int64.shift_right_logical v 32) in
    u32 buf lo;
    u32 buf hi
end

(* The capture host: stash bytes into the staging buffer with a timestamp, keyed
   by kind. Time source is provided externally so the driver loop can advance
   virtual time deterministically. *)

let now_ref = ref 0.0
let staging = Buffer.create (1 lsl 16)
let record_count = ref 0

let write_record kind payload =
  let ts_ms = Int64.of_float !now_ref in
  BE.u8 staging kind;
  BE.u64 staging ts_ms;
  let len = Bytes.length payload in
  BE.u32 staging len;
  Buffer.add_bytes staging payload;
  incr record_count

module CaptureHost = struct
  let ship_backend_cmd payload = write_record kind_be_cmd payload
  let ship_audio_cmd payload = write_record kind_au_cmd payload
end

module Runtime = Regl_runtime.Make (CaptureHost)
