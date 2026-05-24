(* Cross-backend test for dynamic FBO pool growth.

   Ships [start_regl] with [fbo_num = 1] — the smallest legal seed —
   then renders a tree that demands many simultaneous FBO palettes.
   On every frame the renderer is forced to grow the pool past its
   seed:

   - A [composite] needs three palettes at once: left input, right
     input, and the composite's own output. Two of these are atomics
     so each gets its own palette via [drawRenderable].
   - Wrapping the composite in a [group] with N effects forces N more
     ping-pong allocations as each effect acquires a fresh palette.
   - The outer view [group] holds the resulting palette while the
     top-level [palette] passthrough is bound, holding one more.

   Grand total: at least 5 distinct palettes alive across one frame.
   With [fbo_num = 1], the C++ backend logs a warning and grows the
   pool; the JS backend (`getFreePalette` in ml-regl-js/src/app.js)
   already does the same. Both should render identical visible
   output: a colour band that fades over time.

   Same source compiles for either backend; the choice is made in the
   dune executable stanza via the [(libraries ...)] field. *)

open Ml_regl_core
open Ml_regl_core.Regl_proto

type model = { ts : float; frame : int }

let virt_w = 800.0
let virt_h = 600.0

let init () : model * regl_output list =
  let cmds =
    [
      start_regl
        {
          virt_width = virt_w;
          virt_height = virt_h;
          (* Seed the pool at 1. The renderer needs many more
             palettes per frame than this; both backends must grow
             the pool dynamically or the visible output will be
             wrong (sub-trees silently drop). *)
          fbo_num = 1;
          builtin_programs = None;
          window = default_window_config;
        };
      config_regl (ConfigTimeInterval AnimationFrame);
    ]
  in
  ({ ts = 0.0; frame = 0 }, cmds)

let update (m : model) (input : regl_input) :
    model * Regl_audio.audio * regl_output list =
  match input with
  | Regl_proto.Event (Regl_proto.UpdateTick ts) ->
      let frame' = m.frame + 1 in
      ({ ts; frame = frame' }, Regl_audio.silence, [])
  | _ -> (m, Regl_audio.silence, [])

(* Build a renderable that *must* hold more than one palette
   simultaneously. We use a composite of two atomic-bearing groups
   wrapped by a group carrying a chain of effects; effects acquire one
   new palette per pass while the previous result is still alive. *)
let view (m : model) : Regl_common.renderable =
  let tau = 2.0 *. Float.pi in
  let norm_angle a =
    let r = Float.rem a tau in
    if r < 0.0 then r +. tau else r
  in
  let phase = norm_angle (m.ts *. 0.001) in
  let pulse = 0.5 +. (0.5 *. sin phase) in

  (* Two non-trivial sub-trees, each of which the walker turns into a
     single palette via [drawRenderable]. *)
  let left =
    Regl_common.group []
      [
        Regl_builtin_programs.rect (60., 100.) (260., 380.)
          (Color.rgb 0.85 0.30 0.30);
        Regl_builtin_programs.circle (190., 280.) 80.
          (Color.rgb 0.95 0.85 0.20);
      ]
  in
  let right =
    Regl_common.group []
      [
        Regl_builtin_programs.rounded_rect (430., 100.) (320., 380.) 28.
          (Color.rgb 0.20 0.45 0.90);
        Regl_builtin_programs.rect_centered (590., 280.) (220., 140.) phase
          (Color.rgb 0.30 0.85 0.40);
      ]
  in
  (* [linear_fade] is a composite: at draw time the walker holds
     palettes for [left], [right], AND the composite's own output
     palette simultaneously — already 3 palettes alive. *)
  let composed = Regl_compositors.linear_fade pulse left right in
  (* Effects pile on more concurrent palettes — each effect pass
     acquires a fresh palette while the previous one is still bound
     as the source texture. *)
  let effects =
    [
      Regl_effects.color_mult 1.0 0.95 0.85 1.0;
      Regl_effects.alpha_mult 0.9;
      Regl_effects.pixilation 1.4;
    ]
    @ Regl_effects.gblur 0.8
  in
  Regl_common.group []
    [
      Regl_builtin_programs.clear (Color.rgb 0.05 0.07 0.10);
      Regl_common.group effects [ composed ];
    ]

let () = Regl_backend.create_app init update view
