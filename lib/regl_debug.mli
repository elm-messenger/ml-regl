(** Development diagnostics shared by the OCaml application and both hosts.

    Diagnostics are disabled until a host calls [configure]. Applications can
    publish human-readable logs with [log] and one-line JSON snapshots with
    [publish_state]. The host-owned sink keeps the core independent of stdout,
    the browser console, and the eventual MCP WebSocket transport. *)

type level = Debug | Info | Warning | Error

type kind = Log | State

type event = {
  kind : kind;
  level : level;
  payload : string;
}

type sink = event -> unit

val configure : enabled:bool -> sink:sink -> unit
(** Replace the active sink and set whether diagnostics are emitted. *)

val configure_stdout : enabled:bool -> unit
(** Configure the built-in line-oriented stdout sink. *)

val event_to_line : event -> string
(** Render an event using the stable [MCP_LOG] / [MCP_STATE] line format. *)

val set_enabled : bool -> unit
val set_sink : sink -> unit
val enabled : unit -> bool
val reset : unit -> unit
(** Restore the disabled default stdout sink. Primarily useful for tests. *)

val log : ?level:level -> string -> unit
(** Emit a human-readable diagnostic message. *)

val logf :
  ?level:level -> ('a, unit, string, unit) format4 -> 'a

val publish_state : string -> unit
(** Emit a one-line JSON state payload. The function does not parse or validate
    JSON; callers should provide a compact, newline-free JSON string. *)

val publish_statef : ('a, unit, string, unit) format4 -> 'a
