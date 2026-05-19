# API design

## OCaml -> JS (implemented in JS)

- Internal only: backend/audio protobuf batch executors used by `Regl.create_app`.

## JS -> OCaml (implemented in OCaml, exposed as `MlApp` methods)

- `init()`: initialize the app state. The OCaml init callback returns
  `(model * regl_output list)`, and the runtime executes those outputs.
- `update(ts)`: tick. Updates model/audio/backend state only; returns nothing.
- `event(ev)`: forward a DOM event; returns nothing.
- `view()`: asks OCaml to render the current model and returns render protobuf bytes.
- `recvREGLCmdPb(bytes)`: forward a REGL response encoded as protobuf bytes.
- `recvAudioMsgPb(bytes)`: forward an audio backend event encoded as protobuf bytes.

## User update return value

`create_app`'s `init` returns
`('a * regl_output list)`:

1. Initial model
2. Initial side-effect outputs: backend protobuf commands
   (`BackendCommand.t list`)

`create_app`'s `update` returns
`('a * Regl_audio.audio * regl_output list)`:

1. New model
2. Audio description (what should be playing right now). The runtime
   diffs this against the previous frame and emits the appropriate
   start/stop/setVolume/etc. messages.
3. Side-effect outputs: backend protobuf commands (`BackendCommand.t list`)

`create_app` also takes `view : model -> Regl_common.renderable`, which is the
only place rendering is produced.

## Audio module (`Regl_audio`)

- `audio ?config source start_time` — play a source from `start_time` (ms).
- `silence`, `group`, `scale_volume`, `scale_volume_at`, `offset_by`,
  `length`.
- Audio loading now goes through backend protobuf commands rather than the
  audio command batch. The response still comes back as
  `AudioMsg (AudioLoadSuccess { audio_url; source })` or
  `AudioMsg (AudioLoadFailed { audio_url; error })`.
