type level = Debug | Info | Warning | Error
type kind = Log | State
type event = { kind : kind; level : level; payload : string }
type sink = event -> unit

let event_to_line event =
  let level_name =
    match event.level with
    | Debug -> "debug"
    | Info -> "info"
    | Warning -> "warning"
    | Error -> "error"
  in
  let prefix =
    match event.kind with Log -> "MCP_LOG" | State -> "MCP_STATE"
  in
  let line =
    match event.kind with
    | Log -> Printf.sprintf "%s %s %s\n" prefix level_name event.payload
    | State -> Printf.sprintf "%s %s\n" prefix event.payload
  in
  line

let stdout_sink event =
  output_string stdout (event_to_line event);
  flush stdout

let sink_ref : sink ref = ref stdout_sink
let enabled_ref = ref false

let configure ~enabled ~sink =
  enabled_ref := enabled;
  sink_ref := sink

let configure_stdout ~enabled = configure ~enabled ~sink:stdout_sink
let set_enabled value = enabled_ref := value
let set_sink sink = sink_ref := sink
let enabled () = !enabled_ref

let reset () =
  enabled_ref := false;
  sink_ref := stdout_sink

let emit event =
  if !enabled_ref then (
    try !sink_ref event
    with exn ->
      (* A diagnostics sink must never take down the game. *)
      output_string stderr
        (Printf.sprintf "MCP_DEBUG_SINK_ERROR %s\n" (Printexc.to_string exn));
      flush stderr)

let log ?(level = Info) payload = emit { kind = Log; level; payload }

let logf ?(level = Info) format_string =
  Printf.ksprintf (fun payload -> log ~level payload) format_string

let publish_state payload = emit { kind = State; level = Info; payload }
let publish_statef format_string = Printf.ksprintf publish_state format_string
