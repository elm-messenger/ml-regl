(** Text metrics: lay a string out the way the hosts' [textbox] does, to size
    and place text before drawing it (a button fitted to its label, runs of text
    side by side, a number right-aligned next to a label).

    The metrics come from the font's BMFont JSON, the file passed to
    [Regl_proto.load_font]; read it with [Regl_proto.load_file]. *)

type metrics
(** A font's glyph advances, kerning pairs, and line height. *)

val parse_bmfont : string -> (metrics, string) result
(** Parse the BMFont JSON that MSDF generators such as msdf-bmfont-xml write. *)

val line_height : metrics -> float
(** [common.lineHeight] in atlas pixels. A textbox's [size] is the height of one
    line, so glyphs are scaled by [size /. line_height]. *)

val em_size : metrics -> float
(** [info.size] in atlas pixels: a glyph's em is
    [size *. em_size /. line_height]. *)

val has_glyph : metrics -> Uchar.t -> bool

type measured = {
  width : float;  (** the widest line, in virtual units *)
  height : float;  (** lines × size × line_height *)
  lines : float list;  (** each line's width, top to bottom *)
}

val measure :
  ?letter_spacing:float ->
  ?word_spacing:float ->
  ?tab_size:float ->
  ?line_height:float ->
  ?width:float ->
  ?word_break:bool ->
  metrics list ->
  float ->
  string ->
  measured
(** [measure fonts size text] lays [text] out as a textbox with the same options
    would: [fonts] in fallback order (the first gives the width of spaces),
    [size] in virtual units, ["\n"] breaking lines, and with [width] lines
    wrapped at word boundaries ([word_break] also breaks inside words). Defaults
    are those of [Regl_builtin_programs.default_textbox_option]. A glyph that no
    font has counts as zero width. The advance width of a line is where the next
    glyph would start, which is what [align] uses. *)

val measure_textbox :
  (string -> metrics option) ->
  Regl_builtin_programs.textbox_option ->
  measured option
(** [measure_textbox lookup opt] measures [opt.text] with the options of
    [textbox_pro], looking fonts up by name; [None] when a font in [opt.fonts]
    has no metrics. *)
