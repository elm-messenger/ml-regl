open Ml_regl_core

let fail message =
  prerr_endline ("test_regl_debug: " ^ message);
  exit 1

let expect condition message = if not condition then fail message

let () =
  Regl_debug.reset ();
  expect (not (Regl_debug.enabled ())) "debug should start disabled";

  let events = ref [] in
  Regl_debug.configure ~enabled:true ~sink:(fun event -> events := event :: !events);
  Regl_debug.log "started";
  Regl_debug.log ~level:Regl_debug.Warning "slow frame";
  Regl_debug.publish_state {|{"scene":"intro","frame":12}|};
  Regl_debug.publish_statef {|{"score":%d}|} 7;

  let received = List.rev !events in
  expect (List.length received = 4) "all enabled events should reach the sink";
  let first = List.nth received 0 in
  expect (first.kind = Regl_debug.Log) "log kind mismatch";
  expect (first.level = Regl_debug.Info) "default level mismatch";
  expect (first.payload = "started") "log payload mismatch";
  let second = List.nth received 1 in
  expect (second.level = Regl_debug.Warning) "explicit level mismatch";
  let third = List.nth received 2 in
  expect (third.kind = Regl_debug.State) "state kind mismatch";
  expect (third.payload = {|{"scene":"intro","frame":12}|})
    "state payload mismatch";
  let fourth = List.nth received 3 in
  expect (fourth.payload = {|{"score":7}|}) "formatted state mismatch";
  expect
    (Regl_debug.event_to_line fourth =
       {|MCP_STATE {"score":7}
|})
    "state line format mismatch";

  Regl_debug.set_enabled false;
  Regl_debug.log "suppressed";
  expect (List.length !events = 4) "disabled events should be suppressed";
  Regl_debug.reset ();
  expect (not (Regl_debug.enabled ())) "reset should disable debug";
  print_endline "test_regl_debug: ok"
