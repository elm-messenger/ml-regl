open Ml_regl

let mycircle = Regl_builtin_programs.circle (3., 4.) 5. Color.red
let _ = Regl.init_regl ()
