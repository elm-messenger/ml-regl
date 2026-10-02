(* Regl_text against hand-computed layouts. The primary font has line height 20,
   so at size 40 its advances double; the fallback has line height 10. *)
open Ml_regl_core

let primary =
  {|{"info": {"size": 16}, "common": {"lineHeight": 20},
     "chars": [{"id": 32, "xadvance": 5}, {"id": 65, "xadvance": 10},
               {"id": 86, "xadvance": 12}, {"id": 233, "xadvance": 9}],
     "kernings": [{"first": 65, "second": 86, "amount": -2}]}|}

let fallback =
  {|{"common": {"lineHeight": 10},
     "chars": [{"id": 32, "xadvance": 2}, {"id": 8594, "xadvance": 8}]}|}

let get = function Ok m -> m | Error e -> failwith e
let p = get (Regl_text.parse_bmfont primary)
let f = get (Regl_text.parse_bmfont fallback)

let check name ?letter_spacing ?word_spacing ?width ?word_break ?(fonts = [ p ])
    text lines =
  let m =
    Regl_text.measure ?letter_spacing ?word_spacing ?width ?word_break fonts 40.
      text
  in
  if m.lines <> lines then (
    Printf.eprintf "%s: lines %s, expected %s\n" name
      (String.concat ", " (List.map string_of_float m.lines))
      (String.concat ", " (List.map string_of_float lines));
    exit 1);
  assert (m.width = List.fold_left Float.max 0. lines);
  assert (m.height = float_of_int (List.length lines) *. 40.)

let () =
  assert (Regl_text.line_height p = 20.);
  assert (Regl_text.em_size p = 16.);
  assert (Regl_text.em_size f = 10.);
  check "kerning" "AV" [ 40. ];
  (* Kerning applies across spaces, as in the hosts. *)
  check "space" "A V" [ 50. ];
  check "tab" "A\tA" [ 80. ];
  check "newline" "AV\nA" [ 40.; 20. ];
  check "trailing newline" "AV\n" [ 40. ];
  check "empty" "" [];
  check "letter spacing" ~letter_spacing:1. "AA" [ 44. ];
  check "word spacing" ~word_spacing:2. "A A" [ 60. ];
  check "utf-8" "\xc3\xa9" [ 18. ];
  (* No kerning across fonts; spaces come from the first font. *)
  check "fallback" ~fonts:[ p; f ] "A \xe2\x86\x92" [ 62. ];
  check "missing glyph" "AZ" [ 20. ];
  (* Wrapping: a space that overflows ends the line... *)
  check "wrap at space" ~width:45. "AV AV" [ 40.; 40. ];
  (* ...a word that overflows moves to the next line, taking the width of the
     space before it along, as the hosts do... *)
  check "wrap word" ~width:45. "A AV" [ 20.; 40. ];
  (* ...and word_break splits inside words, keeping the kerning already added,
     as the hosts do. *)
  check "word break" ~width:30. ~word_break:true "AVA" [ 16.; 24.; 20. ];
  let opt =
    {
      Regl_builtin_programs.default_textbox_option with
      fonts = [ "main" ];
      text = "AV\nA";
      size = 40.;
      line_height = Some 1.5;
    }
  in
  let lookup = function "main" -> Some p | _ -> None in
  (match Regl_text.measure_textbox lookup opt with
  | Some m -> assert (m.lines = [ 40.; 20. ] && m.height = 120.)
  | None -> assert false);
  assert (
    Regl_text.measure_textbox lookup { opt with fonts = [ "other" ] } = None);
  assert (Result.is_error (Regl_text.parse_bmfont {|{"chars": []}|}));
  assert (
    Result.is_error
      (Regl_text.parse_bmfont {|{"common": {"lineHeight": 20}, "chars": []}|}));
  assert (Result.is_error (Regl_text.parse_bmfont "{"));
  print_endline "test_text_measure: ok"
