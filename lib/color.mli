type rgba = { red : float; green : float; blue : float; alpha : float }
type t = rgba

val rgba : float -> float -> float -> float -> t
val rgb : float -> float -> float -> t
val black : t
val white : t
val red : t
val green : t
val blue : t
val to_rgba : t -> rgba
