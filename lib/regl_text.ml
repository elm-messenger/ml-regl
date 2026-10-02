(* Text layout matching the hosts' textbox program (ml-regl-js src/text.js,
   declgl-desktop textbox_program.cc): advances scaled by size / lineHeight,
   kerning between consecutive glyphs of one font (also across spaces), spaces
   and tabs from the first font's space advance, and the same wrapping. *)

(* A small JSON reader, enough for BMFont files. *)
module Json = struct
  type t =
    | Null
    | Bool of bool
    | Num of float
    | Str of string
    | Arr of t list
    | Obj of (string * t) list

  exception Error of string

  let parse s =
    let n = String.length s in
    let pos = ref 0 in
    let fail msg = raise (Error (Printf.sprintf "%s at byte %d" msg !pos)) in
    let peek () = if !pos < n then s.[!pos] else '\000' in
    let rec skip () =
      if !pos < n then
        match s.[!pos] with
        | ' ' | '\t' | '\n' | '\r' ->
            incr pos;
            skip ()
        | _ -> ()
    in
    let expect c =
      skip ();
      if peek () <> c then fail (Printf.sprintf "expected '%c'" c);
      incr pos
    in
    let literal word value =
      if
        !pos + String.length word <= n
        && String.sub s !pos (String.length word) = word
      then (
        pos := !pos + String.length word;
        value)
      else fail "unexpected token"
    in
    let hex4 () =
      if !pos + 4 > n then fail "short \\u escape";
      let v = int_of_string ("0x" ^ String.sub s !pos 4) in
      pos := !pos + 4;
      v
    in
    let string_ () =
      expect '"';
      let b = Buffer.create 16 in
      let rec go () =
        if !pos >= n then fail "unterminated string";
        let c = s.[!pos] in
        incr pos;
        match c with
        | '"' -> ()
        | '\\' ->
            if !pos >= n then fail "unterminated escape";
            let e = s.[!pos] in
            incr pos;
            (match e with
            | '"' | '\\' | '/' -> Buffer.add_char b e
            | 'b' -> Buffer.add_char b '\b'
            | 'f' -> Buffer.add_char b '\012'
            | 'n' -> Buffer.add_char b '\n'
            | 'r' -> Buffer.add_char b '\r'
            | 't' -> Buffer.add_char b '\t'
            | 'u' ->
                let hi = hex4 () in
                let code =
                  if
                    hi >= 0xD800 && hi <= 0xDBFF
                    && !pos + 6 <= n
                    && s.[!pos] = '\\'
                    && s.[!pos + 1] = 'u'
                  then (
                    pos := !pos + 2;
                    let lo = hex4 () in
                    0x10000 + ((hi - 0xD800) lsl 10) + (lo - 0xDC00))
                  else hi
                in
                let u =
                  if Uchar.is_valid code then Uchar.of_int code else Uchar.rep
                in
                Buffer.add_utf_8_uchar b u
            | _ -> fail "bad escape");
            go ()
        | c ->
            Buffer.add_char b c;
            go ()
      in
      go ();
      Buffer.contents b
    in
    let number () =
      let start = !pos in
      while
        !pos < n
        &&
        match s.[!pos] with
        | '-' | '+' | '.' | 'e' | 'E' | '0' .. '9' -> true
        | _ -> false
      do
        incr pos
      done;
      match float_of_string_opt (String.sub s start (!pos - start)) with
      | Some f -> Num f
      | None -> fail "bad number"
    in
    let rec value () =
      skip ();
      match peek () with
      | '{' ->
          incr pos;
          skip ();
          if peek () = '}' then (
            incr pos;
            Obj [])
          else
            let rec members acc =
              let k = string_ () in
              expect ':';
              let v = value () in
              skip ();
              match peek () with
              | ',' ->
                  incr pos;
                  members ((k, v) :: acc)
              | '}' ->
                  incr pos;
                  Obj (List.rev ((k, v) :: acc))
              | _ -> fail "expected ',' or '}'"
            in
            members []
      | '[' ->
          incr pos;
          skip ();
          if peek () = ']' then (
            incr pos;
            Arr [])
          else
            let rec items acc =
              let v = value () in
              skip ();
              match peek () with
              | ',' ->
                  incr pos;
                  items (v :: acc)
              | ']' ->
                  incr pos;
                  Arr (List.rev (v :: acc))
              | _ -> fail "expected ',' or ']'"
            in
            items []
      | '"' -> Str (string_ ())
      | 't' -> literal "true" (Bool true)
      | 'f' -> literal "false" (Bool false)
      | 'n' -> literal "null" Null
      | _ -> number ()
    in
    let v = value () in
    skip ();
    if !pos <> n then fail "trailing data";
    v

  let member k = function Obj kvs -> List.assoc_opt k kvs | _ -> None
  let num k o = match member k o with Some (Num f) -> Some f | _ -> None
end

type metrics = {
  advances : (int, float) Hashtbl.t;  (** code point -> xadvance *)
  kernings : (int * int, float) Hashtbl.t;
  line_height : float;
  em_size : float;
  space_advance : float;
}

let parse_bmfont text =
  match Json.parse text with
  | exception Json.Error msg -> Error ("font JSON: " ^ msg)
  | json -> (
      let advances = Hashtbl.create 128 in
      let kernings = Hashtbl.create 64 in
      (match Json.member "chars" json with
      | Some (Arr chars) ->
          List.iter
            (fun c ->
              match (Json.num "id" c, Json.num "xadvance" c) with
              | Some id, Some adv ->
                  Hashtbl.replace advances (int_of_float id) adv
              | _ -> ())
            chars
      | _ -> ());
      (match Json.member "kernings" json with
      | Some (Arr ks) ->
          List.iter
            (fun k ->
              match
                (Json.num "first" k, Json.num "second" k, Json.num "amount" k)
              with
              | Some a, Some b, Some amount ->
                  Hashtbl.replace kernings
                    (int_of_float a, int_of_float b)
                    amount
              | _ -> ())
            ks
      | _ -> ());
      let common = Json.member "common" json in
      let info = Json.member "info" json in
      let line_height = Option.bind common (Json.num "lineHeight") in
      match (line_height, Hashtbl.find_opt advances 32) with
      | None, _ -> Error "font JSON: no common.lineHeight"
      | Some lh, _ when lh <= 0. ->
          Error "font JSON: common.lineHeight is not positive"
      | _, None -> Error "font JSON: the font has no space character"
      | Some line_height, Some space_advance ->
          let em_size =
            Option.value
              (Option.bind info (Json.num "size"))
              ~default:line_height
          in
          Ok { advances; kernings; line_height; em_size; space_advance })

let line_height m = m.line_height
let em_size m = m.em_size
let has_glyph m u = Hashtbl.mem m.advances (Uchar.to_int u)

type measured = { width : float; height : float; lines : float list }

(* The browser host's whitespace (JavaScript's \s); '\n' breaks lines. *)
let is_space c =
  (c >= 0x09 && c <= 0x0D)
  || c = 0x20 || c = 0xA0 || c = 0x1680
  || (c >= 0x2000 && c <= 0x200A)
  || c = 0x2028 || c = 0x2029 || c = 0x202F || c = 0x205F || c = 0x3000
  || c = 0xFEFF

let code_points s =
  let rec go i acc =
    if i >= String.length s then Array.of_list (List.rev acc)
    else
      let d = String.get_utf_8_uchar s i in
      let u =
        if Uchar.utf_decode_is_valid d then Uchar.utf_decode_uchar d
        else Uchar.rep
      in
      go (i + Uchar.utf_decode_length d) (Uchar.to_int u :: acc)
  in
  go 0 []

type line = { mutable w : float; mutable glyphs : int }

let measure ?(letter_spacing = 0.) ?(word_spacing = 1.) ?(tab_size = 4.)
    ?(line_height = 1.) ?(width = infinity) ?(word_break = false) fonts size
    text =
  match fonts with
  | [] -> { width = 0.; height = 0.; lines = [] }
  | primary :: _ ->
      let fonts = Array.of_list fonts in
      let cps = code_points text in
      let n = Array.length cps in
      let space = primary.space_advance *. size /. primary.line_height in
      let find c =
        let rec go i =
          if i >= Array.length fonts then None
          else
            match Hashtbl.find_opt fonts.(i).advances c with
            | Some adv -> Some (i, adv)
            | None -> go (i + 1)
        in
        go 0
      in
      let lines = ref [] in
      let line = ref { w = 0.; glyphs = 0 } in
      let cursor = ref 0 in
      let word_cursor = ref 0 in
      let word_width = ref 0. in
      let new_line () =
        lines := !line :: !lines;
        line := { w = 0.; glyphs = 0 };
        word_cursor := !cursor;
        word_width := 0.
      in
      (* The font and code point of the last glyph placed. *)
      let prev = ref None in
      while !cursor < n do
        let c = cps.(!cursor) in
        if c = 0x0A then (
          incr cursor;
          new_line ())
        else
          let ws = is_space c in
          let advance =
            if ws then (
              word_cursor := !cursor + 1;
              word_width := 0.;
              word_spacing *. space *. if c = 0x09 then tab_size else 1.)
            else
              match find c with
              | None -> 0.
              | Some (fi, adv) ->
                  let font = fonts.(fi) in
                  (match !prev with
                  | Some (pf, pc) when pf = fi && !line.glyphs > 0 ->
                      let kern =
                        Option.value
                          (Hashtbl.find_opt font.kernings (pc, c))
                          ~default:0.
                        *. size /. font.line_height
                      in
                      !line.w <- !line.w +. kern;
                      word_width := !word_width +. kern
                  | _ -> ());
                  !line.glyphs <- !line.glyphs + 1;
                  prev := Some (fi, c);
                  (letter_spacing +. adv) *. size /. font.line_height
          in
          !line.w <- !line.w +. advance;
          word_width := !word_width +. advance;
          if !line.w > width && ws then (
            !line.w <- !line.w -. advance;
            new_line ();
            incr cursor)
          else if !line.w > width && word_break && !line.glyphs > 1 then (
            !line.w <- !line.w -. advance;
            !line.glyphs <- !line.glyphs - 1;
            new_line ())
          else if !line.w > width && (not word_break) && !word_width <> !line.w
          then (
            !line.glyphs <- !line.glyphs - (!cursor - !word_cursor + 1);
            cursor := !word_cursor;
            !line.w <- !line.w -. !word_width;
            new_line ())
          else incr cursor
      done;
      (* Like the hosts, drop a last line that has no width. *)
      if !line.w <> 0. then lines := !line :: !lines;
      let widths = List.rev_map (fun l -> l.w) !lines in
      {
        width = List.fold_left Float.max 0. widths;
        height = float_of_int (List.length widths) *. size *. line_height;
        lines = widths;
      }

let measure_textbox lookup (opt : Regl_builtin_programs.textbox_option) =
  let fonts = List.map lookup opt.fonts in
  if List.exists Option.is_none fonts then None
  else
    Some
      (measure ?letter_spacing:opt.letter_spacing ?word_spacing:opt.word_spacing
         ?tab_size:opt.tab_size ?line_height:opt.line_height ?width:opt.width
         ~word_break:opt.word_break
         (List.filter_map Fun.id fonts)
         opt.size opt.text)
