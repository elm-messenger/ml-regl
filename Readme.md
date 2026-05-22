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
