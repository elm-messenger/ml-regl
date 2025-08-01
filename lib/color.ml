type rgba = { red : float; green : float; blue : float; alpha : float }
type t = rgba

let rgba r g b a = { red = r; green = g; blue = b; alpha = a }
let rgb r g b = rgba r g b 1.0
let black = rgb 0.0 0.0 0.0
let white = rgb 1.0 1.0 1.0
let red = rgb 1.0 0.0 0.0
let green = rgb 0.0 1.0 0.0
let blue = rgb 0.0 0.0 1.0
let to_rgba color = color
