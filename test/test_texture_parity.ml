(* Texture orientation parity: draws assets/orientation.png, a 64x64 image of
   4x4 cells in 16 distinct colours, with every texture function and load
   option, each in its own 160x180 slot (8 per row, image at slot origin + (16,
   8), 128x128). The picture must be the same on both hosts, and each slot must
   show the cells listed in [probes]; test/check_texture_parity.py checks both.
   Fonts are drawn too (the labels) and must stay upright.

   Browser: dune build test/test_texture_parity.bc.js, serve the repository
   root, open html/test_texture_parity.html. Desktop: dune build
   test/test_texture_parity_desktop.exe and run it from test/. *)

open Ml_regl_core
open Regl_proto
module P = Regl_builtin_programs

type model = { ready : string list }

let slot i = (float_of_int (i mod 8 * 160), float_of_int (i / 8 * 180))

(* The image's top-right 2x2 cells, as fractions of the texture. *)
let crop_pos = (0.5, 0.)
let crop_size = (0.5, 0.5)

(* Draws the texture named by the [tex] field into a rectangle; the rectangle's
   top-left corner samples uv (0, 1), as the built-ins do. *)
let textured_program : Regl_program.regl_program =
  {
    frag =
      {|
precision mediump float;
uniform sampler2D tex;
varying vec2 vuv;
void main() { gl_FragColor = texture2D(tex, vuv); }
|};
    vert =
      {|
precision mediump float;
attribute vec2 uv;
uniform vec4 posize;
uniform vec2 view;
uniform vec4 camera;
varying vec2 vuv;
void main() {
  vuv = vec2(uv.x, 1.0 - uv.y);
  vec2 diff = posize.xy + uv * posize.zw - camera.xy;
  float c = cos(camera.w);
  float s = sin(camera.w);
  vec2 rotated = vec2(c * diff.x + s * diff.y, -s * diff.x + c * diff.y);
  gl_Position = vec4(rotated * camera.z / view, 0.0, 1.0);
}
|};
    attributes =
      Some
        [
          ("uv", Regl_program.static_numbers [ 0.; 0.; 1.; 0.; 1.; 1.; 0.; 1. ]);
        ];
    uniforms =
      Some
        [
          ("posize", DynamicValue "posize"); ("tex", DynamicTextureValue "tex");
        ];
    elements = Some (Regl_program.static_numbers [ 0.; 1.; 2.; 0.; 2.; 3. ]);
    primitive = None;
    count = None;
  }

(* Effects and compositors that pass their input through unchanged. *)
let identity_effect =
  Regl_program.make_effect_simple
    {|
precision mediump float;
uniform sampler2D texture;
varying vec2 vuv;
void main() { gl_FragColor = texture2D(texture, vuv); }
|}
    []

let first_compositor =
  Regl_program.make_compositor_simple
    {|
precision mediump float;
uniform sampler2D t1;
uniform sampler2D t2;
varying vec2 vuv;
void main() { gl_FragColor = texture2D(t1, vuv); }
|}
    []

let programs =
  [
    ("textured", textured_program);
    ("identity", identity_effect);
    ("first", first_compositor);
  ]

let nearest = { default_texture_options with mag = Some MagNearest }
let cut = { nearest with crop = Some ((32, 0), (32, 32)) }

let init () =
  ( { ready = [] },
    [
      start_regl
        {
          virt_width = 1280.;
          virt_height = 720.;
          fbo_num = 4;
          builtin_programs = None;
          window = default_window_config;
          app_name = None;
        };
      config_regl (ConfigTimeInterval AnimationFrame);
      load_font "consolas" "assets/Consolas.png" "assets/Consolas.json";
      load_texture "full" "assets/orientation.png" (Some nearest);
      load_texture "flipped" "assets/orientation.png"
        (Some { nearest with flip_y = true });
      load_texture "cut" "assets/orientation.png" (Some cut);
      load_texture "cut_flipped" "assets/orientation.png"
        (Some { cut with flip_y = true });
    ]
    @ List.map
        (fun (name, program) ->
          create_regl_program ~shader_language:GlslEs100 name program)
        programs )

let update m = function
  | REGLRecvMsg (REGLTextureLoaded t) ->
      ({ ready = t.name :: m.ready }, Regl_audio.silence, [])
  | REGLRecvMsg (REGLFontLoaded name | REGLProgramCreated name) ->
      ({ ready = name :: m.ready }, Regl_audio.silence, [])
  | _ -> (m, Regl_audio.silence, [])

(* Each probe draws into the 128x128 square at [(x, y)]. *)
let probes :
    (string * string list * (float * float -> Regl_common.renderable)) list =
  let size = (128., 128.) in
  let corners (x, y) =
    ((x, y), (x +. 128., y), (x +. 128., y +. 128.), (x, y +. 128.))
  in
  let center (x, y) = (x +. 64., y +. 64.) in
  [
    ("rect_texture", [ "full" ], fun p -> P.rect_texture p size "full");
    ( "rect_texture alpha",
      [ "full" ],
      fun p -> P.rect_texture_with_alpha p size 1. "full" );
    ( "centered_texture",
      [ "full" ],
      fun p -> P.centered_texture (center p) size 0. "full" );
    ( "centered alpha",
      [ "full" ],
      fun p -> P.centered_texture_with_alpha (center p) size 0. 1. "full" );
    ( "texture",
      [ "full" ],
      fun p ->
        let a, b, c, d = corners p in
        P.texture a b c d "full" );
    ( "texture alpha",
      [ "full" ],
      fun p ->
        let a, b, c, d = corners p in
        P.texture_with_alpha a b c d 1. "full" );
    ( "rect_texture_cropped",
      [ "full" ],
      fun p -> P.rect_texture_cropped p size crop_pos crop_size "full" );
    ( "rect cropped alpha",
      [ "full" ],
      fun p ->
        P.rect_texture_cropped_with_alpha p size crop_pos crop_size 1. "full" );
    ( "centered cropped",
      [ "full" ],
      fun p ->
        P.centered_texture_cropped (center p) size 0. crop_pos crop_size "full"
    );
    ( "centered cropped alpha",
      [ "full" ],
      fun p ->
        P.centered_texture_cropped_with_alpha (center p) size 0. crop_pos
          crop_size 1. "full" );
    ( "texture_cropped",
      [ "full" ],
      fun p ->
        let a, b, c, d = corners p in
        P.texture_cropped a b c d (0.5, 1.) (1., 1.) (1., 0.5) (0.5, 0.5) "full"
    );
    ( "texture_cropped alpha",
      [ "full" ],
      fun p ->
        let a, b, c, d = corners p in
        P.texture_cropped_with_alpha a b c d (0.5, 1.) (1., 1.) (1., 0.5)
          (0.5, 0.5) 1. "full" );
    ("crop at load", [ "cut" ], fun p -> P.rect_texture p size "cut");
    ("flip_y", [ "flipped" ], fun p -> P.rect_texture p size "flipped");
    ( "crop at load + flip_y",
      [ "cut_flipped" ],
      fun p -> P.rect_texture p size "cut_flipped" );
    ( "flip_y + draw crop",
      [ "flipped" ],
      fun p -> P.rect_texture_cropped p size crop_pos crop_size "flipped" );
    ( "custom program",
      [ "full"; "textured" ],
      fun p ->
        let x, y = p in
        Regl_common.atomic "textured"
          [
            Regl_common.nums "posize" [ x; y; 128.; 128. ];
            Regl_common.str "tex" "full";
          ] );
    ( "custom effect",
      [ "full"; "identity" ],
      fun p ->
        Regl_common.group
          [ Regl_common.mk_effect "identity" [] ]
          [ P.rect_texture p size "full" ] );
    ( "built-in effect",
      [ "full" ],
      fun p ->
        Regl_common.group
          [ Regl_effects.alpha_mult 1. ]
          [ P.rect_texture p size "full" ] );
    ( "custom compositor",
      [ "full"; "first" ],
      fun p ->
        Regl_common.composite "first" []
          (P.rect_texture p size "full")
          (P.rect_texture p size "flipped") );
    ( "built-in compositor",
      [ "full" ],
      fun p ->
        Regl_compositors.linear_fade 0.5
          (P.rect_texture p size "full")
          (P.rect_texture p size "full") );
  ]

let view m =
  let ready names = List.for_all (fun n -> List.mem n m.ready) names in
  Regl_common.group []
    (P.clear (Color.rgb 0.5 0.5 0.5)
    :: List.concat
         (List.mapi
            (fun i (label, needs, draw) ->
              let x, y = slot i in
              [
                (if ready needs then draw (x +. 16., y +. 8.) else P.empty);
                (if ready [ "consolas" ] then
                   P.textbox
                     (x +. 16., y +. 142.)
                     12. label "consolas" Color.white
                 else P.empty);
              ])
            probes))

let () = Regl_backend.create_app init update view
