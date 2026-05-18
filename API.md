# API design

## OCaml -> JS (implemented in JS)

- `execCmd`: execute REGL commands (textures / fonts / programs / config / start)
- `execAudioCmdPb`: execute an audio command batch encoded as protobuf bytes.

## JS -> OCaml (implemented in OCaml, exposed as `MlApp` methods)

- `update(ts)`: tick. Returns the renderable JS object.
- `event(ev)`: forward a DOM event.
- `recvREGLCmd(msg)`: forward a REGL response (texture/font/program loaded).
- `recvAudioMsgPb(bytes)`: forward an audio backend event encoded as protobuf bytes.

## User update return value

`create_app`'s `update` returns
`('a, Regl_common.renderable, Regl_audio.audio, regl_output list)`:

1. New model
2. Renderable (what to draw this frame)
3. Audio description (what should be playing right now). The runtime
   diffs this against the previous frame and emits the appropriate
   start/stop/setVolume/etc. messages.
4. Side-effect outputs: REGL commands (`StartREGL`, `LoadTexture`, ...)
   and audio loads (`LoadAudio url`).

## Audio module (`Regl_audio`)

- `audio ?config source start_time` — play a source from `start_time` (ms).
- `silence`, `group`, `scale_volume`, `scale_volume_at`, `offset_by`,
  `length`.
- `LoadAudio url` triggers a fetch + decode; the response comes back as
  `AudioMsg (AudioLoadSuccess { audio_url; source })` or
  `AudioMsg (AudioLoadFailed { audio_url; error })`.
