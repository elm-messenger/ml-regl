# ml-regl

A declarative 2D renderer for OCaml. Write your app once; run it in the browser
on WebGL or as a native desktop app on OpenGL.

Inspired by Elm's [`ml-regl`](https://github.com/elm-messenger/ml-regl).

## Architecture

```
                ┌──────────────────────────────────┐
                │   Your OCaml app (Elm-style)     │
                │   init / update / view           │
                └──────────────────────────────────┘
                                  │  renderable + audio + outputs
                                  ▼
                ┌──────────────────────────────────┐
                │   ml_regl_core (this repo, lib/) │
                │   runtime + protobuf transport   │
                └──────────────────────────────────┘
                       │ bytes              │ bytes
            ┌──────────┴                    │
            ▼                               ▼
   ┌──────────────────┐    ┌──────────────────────────────┐
   │  Regl_js host    │    │  Regl_desktop host           │
   │  (ml-regl-js/)   │    │  (declgl-desktop/, C++/SDL3) │
   │  WebGL via REGL  │    │  OpenGL 3.3 Core             │
   └──────────────────┘    └──────────────────────────────┘
```

- **OCaml core (`lib/`)**: pure, host-agnostic. Defines the renderable tree,
  audio tree, and runtime state machine. Talks to hosts only via protobuf
  bytes (see `lib/proto/`).
- **JS host (`ml-regl-js/`)**: Browserify bundle that hosts the OCaml app
  compiled with `js_of_ocaml`. Renders with [`regl`](https://github.com/regl-project/regl)
  on a `<canvas>`.
- **Desktop host (`declgl-desktop/`)**: C++17 / SDL3 / OpenGL 3.3 native
  runtime, linked into OCaml via `caml_callback`. Drop-in replacement for
  the JS host.

The two hosts agree on the wire format (protos in `lib/proto/`) and on the
input vocabulary (SDL keycode names, 1-based mouse buttons), so OCaml apps
do not branch on backend.

## Features

- **Declarative rendering**: `view : model -> renderable`. The runtime
  diffs and ships, the host draws.
- **Built-in programs**: rect, rounded-rect, circle, triangle, polygon,
  texture (centered / cropped), textbox (MSDF), and custom dynamic shaders.
- **Effects & compositors**: alpha-mult, color-mult, blur, gblur, outline,
  pixilation, CRT, FXAA, fade compositors, palette swap, custom shaders.
- **Camera-aware groups** with virtual canvas coordinates.
- **FBO pool** with seed size + dynamic growth (warn → grow → hard cap).
- **Declarative audio** (`Regl_audio`): describe what *should* be playing;
  the runtime diffs and emits start / stop / setVolume.
- **Asset loading**: textures, MSDF fonts, audio buffers — all async, all
  reported back to OCaml as events.
- **Cross-backend tests** in `test/` (one source, two binaries).

## Quickstart

OCaml app entry point:

```ocaml
let () =
  Regl_js.create_app init update view  (* or Regl_desktop.create_app *)
```

### JS

```bash
cd ml-regl-js
pnpm i
make build              # writes build/regl.js
```

### Desktop

```bash
cd declgl-desktop
cmake --preset mac-debug
cmake --build --preset mac-debug
DECLGL_BUILD_DIR=$PWD/build/mac-debug dune build  # from repo root
```

See `declgl-desktop/Readme.md` for full toolchain setup (vcpkg, SDL3, etc.).

## Development diagnostics

The portable core exposes `Regl_debug.log` and `Regl_debug.publish_state` for
development-only diagnostics. A host supplies the sink, so application code
does not depend on stdout, browser APIs, or MCP transport details:

```ocaml
Regl_debug.log "entered forest_intro";
Regl_debug.publish_state
  {|{"scene":"forest_intro","objective":"find_key"}|};
```

Desktop diagnostics are enabled with `DECLGL_DEBUG=1` and are emitted as
flushed `MCP_LOG ...` / `MCP_STATE ...` lines. Set
`DECLGL_CONTROL_URL=ws://127.0.0.1:PORT` as well to connect the optional JSON
control channel. Browser diagnostics are enabled with `?debug=1`, `?mcp=1`, or
an `#mcp=...` URL fragment. A WebSocket URL can be supplied as
`?control=ws://...` or `#mcp=ws://...`.

The control connection is outbound from the game host, so the MCP process
owns the localhost listener. Both hosts use the same JSON envelopes:

```json
{"type":"hello","protocol":1,"runtime":"ml-regl-desktop"}
{"method":"pause","id":1}
{"type":"response","id":1,"ok":true,"result":{"paused":true}}
```

Supported commands are `pause`, `resume`, `step` (optional `frames` and
`dt_ms`), `set_time`, `get_state`, `get_render_tree`, `screenshot`, and
`input` (`key_down`, `key_up`, `mouse_down`, `mouse_up`, or
`mouse_move`). Commands are applied at frame boundaries on the render thread.
State/log/frame events use `type: "state"`, `"log"`, and `"frame"`; protobuf
remains the internal renderer and game-event protocol. Without the relevant
debug/control flag, diagnostics and remote control are disabled.
The standalone contract is documented in [`docs/ControlProtocol.md`](docs/ControlProtocol.md).
The native integration smoke test uses the existing `test_fps_smoke` program:
`python3 test/native_control_smoke.py`.

## Repository layout

| Path                | Contents                                       |
| ------------------- | ---------------------------------------------- |
| `lib/`              | OCaml core: runtime, proto, renderable, audio  |
| `lib/backend/js/`   | `Regl_js` facade (`Js_of_ocaml`)               |
| `lib/backend/desktop/` | `Regl_desktop` facade + libdeclgl glue      |
| `lib/proto/`        | Protobuf schemas (canonical copy)              |
| `ml-regl-js/`       | Browser host (REGL/WebGL)                      |
| `declgl-desktop/`   | Native host (C++ / SDL3 / OpenGL 3.3)          |
| `test/`             | Cross-backend test apps                        |
| `html/`             | Browser test harnesses                         |

## License

See `LICENSE`.
