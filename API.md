# API design

## OCaml -> JS (implemented in JS)

- `execCmd`: execute REGL commands (textures / fonts / programs / config / start)
- `execAudioCmd`: execute audio commands. Payload is `{ actions, loads }`
  where `actions` is a list of audio diff messages
  (`startSound` / `stopSound` / `setVolume` / `setVolumeAt` /
  `setLoopConfig` / `setPlaybackRate`) and `loads` is a list of
  `{ requestId, audioUrl }`.

## JS -> OCaml (implemented in OCaml, exposed as `MlApp` methods)

- `update(ts)`: tick. Returns the renderable JS object.
- `event(ev)`: forward a DOM event.
- `recvREGLCmd(msg)`: forward a REGL response (texture/font/program loaded).
- `recvAudioMsg(msg)`: forward an audio response. Shapes:
  - `{ _c: "audioContextReady", sampleRate }`
  - `{ _c: "audioLoadSuccess", requestId, bufferId, duration }` (duration in seconds)
  - `{ _c: "audioLoadFailed",  requestId, error }`

## User update return value

`create_app`'s `update` returns
`('a, Regl_common.renderable, Regl_audio.audio, regl_output list)`:

1. New model
2. Renderable (what to draw this frame)
3. Audio description (what should be playing right now). The runtime
   diffs this against the previous frame and emits the appropriate
   start/stop/setVolume/etc. messages.
4. Side-effect outputs: REGL commands (`StartREGL`, `LoadTexture`, ...)
   and audio loads (`LoadAudio (request_id, url)`).

## Audio module (`Regl_audio`)

- `audio ?config source start_time` — play a source from `start_time` (ms).
- `silence`, `group`, `scale_volume`, `scale_volume_at`, `offset_by`,
  `length`.
- `LoadAudio (request_id, url)` triggers a fetch + decode; the response
  comes back as `AudioMsg (AudioLoadSuccess { request_id; source })` or
  `AudioMsg (AudioLoadFailed { request_id; error })`.
