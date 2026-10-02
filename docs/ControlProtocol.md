# ml-regl Control Protocol

The optional development control channel is one JSON message per WebSocket
text frame. The MCP process owns the localhost listener. A native or browser
game host connects outbound and sends a hello message after the connection
opens.

## Connection

Desktop:

```text
DECLGL_DEBUG=1 DECLGL_CONTROL_URL=ws://127.0.0.1:8765 ./game
```

DECLGL_REMOTE_CONTROL=1 can be used instead of DECLGL_DEBUG=1 when logs should
not be enabled. Browser URLs accept ?control=ws://... or #mcp=ws://...; the URL
fragment is preferred for tokens.

## Messages

Hello:

```json
{
  "type": "hello",
  "protocol": 1,
  "runtime": "ml-regl-desktop",
  "capabilities": ["pause", "resume", "quit", "step", "set_time",
                   "get_state", "get_render_tree", "screenshot", "input"]
}
```

Commands use method, an optional caller-supplied id, and an optional params
object:

```json
{"method":"step","id":"next","params":{"frames":1,"dt_ms":16.6667}}
```

Every command is applied on the game/render thread at a frame boundary and
receives a response:

```json
{"type":"response","id":"next","ok":true,"result":{"queued":1}}
```

Errors use ok: false and an error object. Unknown or malformed commands do not
terminate the game.

## Commands

- pause, resume, and quit control lifecycle.
- step pauses the game and advances a requested number of frames. dt_ms selects
  the deterministic clock increment. The first step switches the game to the
  deterministic clock, which continues from the game's current time unless
  set_time was used.
- set_time sets the deterministic clock in milliseconds.
- get_state returns the latest publish_state payload, recent logs, frame number,
  clock, and pause status.
- get_render_tree returns the latest render tree as JSON.
- screenshot captures the current frame: the desktop back buffer as a file
  (path) or the browser canvas as a data URL (data_url). Optional params:
  - area: "window" (default; the whole desktop window, letterbox included)
    or "view" (only the virtual area).
  - region: {x, y, width, height} in virtual units, a part of the view.
  - scale: "native" (default; captured pixels) or "virtual" (one pixel per
    virtual unit, never scaled up).
  - max_width: largest output width in pixels; the aspect ratio is kept.
  - format: "bmp" (desktop default), "png" (browser default) or "jpeg"; the
    browser writes PNG for "bmp".
  - quality: JPEG quality from 1 to 100 (default 90).
  - path (desktop): where to write the file; default mcp_frame_N.<ext> in
    the game's working directory.

  The result also gives format, width and height of the image, view (the
  virtual area in window or canvas pixels), virtual (the virtual size), and
  pixels_per_unit (image pixels per virtual unit). Images are opaque, as the
  window shows them.
- input injects a key_down, key_up, mouse_down, mouse_up, or mouse_move event.
  Key codes use the SDL naming convention and mouse buttons are one-based.

## Events

The host may send asynchronous events at any time after hello:

```json
{"type":"log","level":"info","message":"entered intro"}
{"type":"state","state":{"scene":"intro","frame":12}}
{"type":"frame","frame":12,"time_ms":200.0}
```

printf remains a human-readable stdout diagnostic. Regl_debug.log and
Regl_debug.publish_state are opt-in and use the same event stream; protobuf
continues to carry renderer and game input data internally.
