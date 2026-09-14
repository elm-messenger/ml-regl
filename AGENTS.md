# AGENTS.md

## Purpose and scope

`ml-regl` is the backend-neutral declarative rendering and audio layer used by
the sibling `../ml-messenger` framework. It owns the public OCaml description
types, protobuf encoding and decoding, runtime diffing, and the virtual backend
entry point. The actual browser and desktop players are separate repositories
checked out inside this worktree.

Keep responsibilities separated:

- `lib/` owns portable OCaml APIs and state machines. It must remain usable
  without `js_of_ocaml` or the desktop C++ toolchain.
- `lib/backend/js/` and `lib/backend/desktop/` are thin host adapters. Shared
  lifecycle behavior belongs in `Regl_runtime.Make`, not in both adapters.
- `ml-regl-js/` owns browser APIs, REGL/WebGL, Web Audio, DOM input, and browser
  persistence/fetch behavior.
- `declgl-desktop/` owns SDL3, OpenGL, native audio, native asset I/O, and the
  OCaml/C++ bridge.
- `../ml-messenger` owns scenes, components, message routing, resource names,
  and framework-level audio channel bookkeeping.

Do not add backend checks to portable application or framework code. A host
behavior change should preserve parity unless the platform fundamentally
cannot provide the feature and that limitation is documented.

## Repository boundaries

This checkout contains nested Git repositories and protocol checkouts:

- `ml-regl-js/` is the browser host repository and has its own `AGENTS.md`.
- `declgl-desktop/` is the native host repository and has its own `AGENTS.md`.
- `lib/proto/`, `ml-regl-js/proto/`, and `declgl-desktop/proto/` are checkouts
  of the shared `declgl-pb` schema repository.

Treat changes in each repository as independent. Inspect status and diffs with
`git -C <path> status --short` and `git -C <path> diff`; a top-level diff does
not show the content of a nested repository. Do not commit a changed gitlink
unless the corresponding nested commit exists and the pointer update is part
of the requested change.

## Repository map

- `lib/regl_common.*`: renderable tree constructors and frame encoding.
- `lib/regl_builtin_programs.*`: typed constructors for built-in draw calls.
- `lib/regl_program.*`: custom shader/program descriptions and encoders.
- `lib/regl_effects.*`, `lib/regl_compositors.*`: effect and compositor
  constructors layered over `Regl_common`.
- `lib/regl_audio.*`: declarative audio tree, flattening, diff state, audio
  action encoding, and audio event decoding.
- `lib/regl_proto.*`: backend commands, resource operations, input events, and
  protobuf translation.
- `lib/regl_runtime.*`: shared init/update/view driver. It owns the previous
  audio state and ships only the command batches produced by each update.
- `lib/regl_debug.*`: host-installed portable diagnostics sink.
- `lib/backend/shared/`: virtual `Regl_backend.create_app` contract.
- `lib/backend/js/`: Js_of_ocaml exports and implementation of the virtual
  backend.
- `lib/backend/desktop/`: native callbacks, C stubs, and implementation of the
  virtual backend.
- `lib/proto/`: shared protobuf schemas consumed by Dune code generation.
- `test/`: portable OCaml smoke apps, usually paired as browser and desktop
  executables from one source file.
- `html/`: browser harnesses for the compiled test bundles.
- `docs/ControlProtocol.md`: optional JSON debug/control channel contract. It
  is separate from the protobuf renderer protocol.

The public packages are `ml_regl_core`, the virtual `regl_backend`, and its
`regl_js` and `regl_desktop` implementations. Public `.mli` files define the
supported OCaml surface; update implementation and interface together.

## Core invariants

- `ml_regl_core` stays portable. Never introduce a `Js_of_ocaml`, SDL, OpenGL,
  browser, or OS dependency into `lib/`.
- Link an application with exactly one implementation of the virtual
  `regl_backend` library.
- Renderables and audio values are declarative descriptions. The shared
  runtime turns them into protobuf bytes; hosts must not infer OCaml state.
- `Regl_runtime.Make.drive` updates the model, ships backend outputs, diffs the
  new audio tree against its prior state, and then ships audio actions. Keep
  that ordering and retain the diff state between all input kinds.
- An audio voice is identified by buffer, effective absolute start time, and
  `start_at`. Volume, volume timelines, loop configuration, and playback rate
  are incremental updates. A changed identity is a stop plus a new start.
- Runtime timestamps and audio offsets, loop points, and `start_at` values are
  milliseconds. `Regl_audio.source.duration` is seconds.
- Hosts receive 1-based mouse buttons and the shared SDL-style key vocabulary.
  Preserve that contract when touching either adapter.
- Asset loads are asynchronous host operations. Success and failure must both
  round-trip as backend events; do not manufacture success in the OCaml core.
- Debug/control commands are applied at frame boundaries. Keep the JSON
  control path optional and independent of the protobuf transport.

## Protocol changes

The schema repository is the source of truth. Protocol changes must be
implemented end to end across the OCaml core and both hosts.

- Prefer additive fields and new `oneof` cases. Never reuse field numbers or
  silently change the meaning or units of an existing field.
- Keep all three protocol checkouts on the same schema commit before testing.
- Do not hand-edit generated protobuf code. Dune generates OCaml modules in
  the build tree, `ml-regl-js/Makefile` generates `src/generated/mlregl_pb.js`,
  and CMake generates C++ protobuf sources in its build tree.
- Update encoders, decoders, host dispatch, and tests together. A schema that
  only one backend understands is incomplete.

## Build and verification

Run commands from this repository root unless noted otherwise.

```sh
# Portable unit test and formatting check.
dune runtest
dune build @fmt

# Compile a representative browser target without the desktop toolchain.
dune build test/test_ml_regl.bc.js

# Build the browser host bundle and its non-browser control test.
pnpm --dir ml-regl-js install
make -C ml-regl-js build
make -C ml-regl-js test-control
```

For a native build, first configure and build `declgl-desktop`, then point Dune
at the resulting archive directory:

```sh
cd declgl-desktop
cmake --preset linux-debug
cmake --build --preset linux-debug
cd ..
DECLGL_BUILD_DIR=$PWD/declgl-desktop/build/linux-debug \
  dune build test/test_ml_regl_desktop.exe
```

Use the preset for the current platform. `DECLGL_BUILD_DIR` must contain both
`libdeclgl.a` and `declgl_link_flags.sexp`. Do not run a native smoke executable
as a routine unit test: most open an SDL window and run until their configured
timeout or a quit event.

Match verification to the change:

- Audio: compile `test/test_audio_smoke.bc.js` and the corresponding desktop
  executable when the native toolchain is available; listen to both for an
  actual behavior claim.
- Rendering/effects/textures/fonts: use the smallest relevant `html/` harness
  and desktop smoke app. Compilation alone does not establish visual parity.
- Input, frame pacing, storage, and control: use their named smoke targets;
  `python3 test/native_control_smoke.py` exercises the native control channel.
- Browser harnesses require HTTP serving because resources and bundles are
  fetched by URL. Serve the repository root so the existing absolute paths
  resolve.

## Style and change discipline

- Use the repository `.ocamlformat` profile and run formatting only on OCaml
  files in scope.
- Prefer the typed smart constructors in public modules over constructing
  generated protobuf records in application-facing code.
- Keep `.ml` and `.mli` declarations synchronized. Avoid exposing internal
  diff or transport representation accidentally.
- `dune-project` generates the opam files. Change package metadata there, then
  regenerate; do not maintain generated opam files by hand.
- Do not edit `_build`, browser `build/`, any CMake `build/` directory,
  generated protobuf output, or generated embedded shader sources.
- Warnings are not configured as errors everywhere, but new warnings are still
  defects.
- Keep fixes narrowly scoped and preserve unrelated user changes. Follow the
  existing Conventional Commit subjects (`feat:`, `fix:`, `chore:`) when asked
  to commit.

Before finishing, inspect the diff and status in every repository touched.
Report which unit, compile, browser, desktop, visual, or audio checks were and
were not performed.
