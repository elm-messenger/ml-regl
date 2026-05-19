open Ml_regl
open Js_of_ocaml

(* Demo: load a texture and an audio source, render the image, click to play. *)

type sound_state =
  | Loading
  | Loaded of Regl_audio.source
  | Failed

type model = {
  current_ts : float;  (** absolute ms from latest Tick *)
  texture_loaded : bool;
  sound : sound_state;
  play_at : float option;  (** absolute ms; if Some, that's when to start *)
}

let initial_model =
  {
    current_ts = 0.0;
    texture_loaded = false;
    sound = Loading;
    play_at = None;
  }

let texture_name = "enemy"
let texture_url = "test/assets/enemy.png"
let audio_url = "test/assets/test.ogg"

let view (m : model) =
  let bg = Regl_builtin_programs.clear Color.white in
  let img =
    if m.texture_loaded then
      [
        Regl_builtin_programs.centered_texture (960., 540.) (256., 256.) 0.0
          texture_name;
      ]
    else
      [
        Regl_builtin_programs.textbox_centered (960., 540.) 40.
          "Loading texture..." "consolas" Color.black;
      ]
  in
  let status =
    let msg =
      match m.sound with
      | Loading -> "Audio: loading..."
      | Loaded _ ->
          if m.play_at <> None then "Audio: playing (click to retrigger)"
          else "Audio: ready (click to play)"
      | Failed -> "Audio: failed to load"
    in
    Regl_builtin_programs.textbox_centered (960., 900.) 40. msg "consolas"
      Color.black
  in
  Regl_common.group [] ([ bg ] @ img @ [ status ])

let audio (m : model) : Regl_audio.audio =
  match (m.sound, m.play_at) with
  | Loaded src, Some t -> Regl_audio.audio src t
  | _ -> Regl_audio.silence

let init (canvas : Dom_html.canvasElement Js.t option) =
  let startconfig : Regl.regl_start_config =
    {
      virt_width = 1920.;
      virt_height = 1080.;
      fbo_num = 5;
      builtin_programs = None;
    }
  in
  let mc = Option.get canvas in
  mc##.width := 1280;
  mc##.height := 720;
  ( initial_model,
    [
      Regl.start_regl startconfig;
      Regl.load_texture texture_name texture_url None;
      Regl.load_audio audio_url;
    ] )

let update (_canvas : Dom_html.canvasElement Js.t option) (m : model)
    (e : Regl.regl_input) =
  match e with
  | Regl.Tick ts ->
      let nm = { m with current_ts = ts } in
      (nm, audio nm, [])
  | Regl.Event event ->
      let ty = Js.to_string event##._type in
      let nm =
        if ty = "click" || ty = "mousedown" || ty = "touchstart" then
          match m.sound with
          | Loaded _ ->
              (* Use the latest Tick timestamp as the absolute clock. *)
              let now = m.current_ts in
              { m with play_at = Some now }
          | _ -> m
        else m
      in
      (nm, audio nm, [])
  | Regl.REGLRecvMsg msg ->
      let nm =
        match msg with
        | Regl.REGLTextureLoaded t when t.name = texture_name ->
            { m with texture_loaded = true }
        | _ -> m
      in
      (nm, audio nm, [])
  | Regl.AudioMsg msg ->
      let nm =
        match msg with
        | Regl.AudioLoadSuccess { audio_url = url; source }
          when url = audio_url ->
            { m with sound = Loaded source }
        | Regl.AudioLoadFailed { audio_url = url; _ } when url = audio_url ->
            { m with sound = Failed }
        | _ -> m
      in
      (nm, audio nm, [])

let _ = Regl.create_app init update view
