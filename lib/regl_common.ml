open Js_of_ocaml

type program_call = (string * Js.Unsafe.any) list
type regl_effect = program_call
type camera = { x : float; y : float; zoom : float; rotation : float }

type renderable =
  | AtomicRenderable of program_call
  | GroupRenderable of regl_effect list * renderable list
  | GroupRenderableWithCamera of camera * regl_effect list * renderable list

let rec render = function
  | AtomicRenderable value ->
      let pairs = List.map (fun (k, v) -> (k, v)) value in
      Js.Unsafe.obj (Array.of_list pairs)
  | GroupRenderable (effects, renderables) ->
      let effects_array =
        Array.of_list
          (List.map (fun e -> Js.Unsafe.obj (Array.of_list e)) effects)
      in
      let renderables_array = Array.of_list (List.map render renderables) in
      Js.Unsafe.obj
        [|
          ("e", Js.Unsafe.inject (Js.array effects_array));
          ("c", Js.Unsafe.inject (Js.array renderables_array));
          ("_c", Js.Unsafe.inject (Js.number_of_float 2.0));
        |]
  | GroupRenderableWithCamera (camera, effects, renderables) ->
      let effects_array =
        Array.of_list
          (List.map (fun e -> Js.Unsafe.obj (Array.of_list e)) effects)
      in
      let renderables_array = Array.of_list (List.map render renderables) in
      let camera_array =
        [| camera.x; camera.y; camera.zoom; camera.rotation |]
      in
      Js.Unsafe.obj
        [|
          ("e", Js.Unsafe.inject (Js.array effects_array));
          ("c", Js.Unsafe.inject (Js.array renderables_array));
          ("_c", Js.Unsafe.inject (Js.number_of_float 2.0));
          ( "_sc",
            Js.Unsafe.inject
              (Js.array (Array.map Js.number_of_float camera_array)) );
        |]

let group effects renderables = GroupRenderable (effects, renderables)

let group_with_camera camera effects renderables =
  GroupRenderableWithCamera (camera, effects, renderables)

let update_list_foldr key new_val list =
  let rec aux acc found = function
    | [] -> if found then acc else (key, new_val) :: acc
    | (k, v) :: rest when k = key -> aux ((k, new_val) :: acc) true rest
    | (k, v) :: rest -> aux ((k, v) :: acc) found rest
  in
  aux [] false (List.rev list)

let update_field key value = function
  | AtomicRenderable ov -> AtomicRenderable (update_list_foldr key value ov)
  | r -> r

let get_field key = function
  | AtomicRenderable pc ->
      let rec aux = function
        | [] -> None
        | (k, v) :: _ when k = key -> Some v
        | _ :: rest -> aux rest
      in
      aux pc
  | _ -> None

let gen_prog pc = AtomicRenderable pc

let to_rgba_list color =
  let rgba = Color.to_rgba color in
  [ rgba.red; rgba.green; rgba.blue; rgba.alpha ]
