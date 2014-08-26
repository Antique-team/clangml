let identity x = x

let simplify_bool = identity
let simplify_sloc = identity
let simplify_string = identity
let simplify_int = identity
let simplify_char = identity
let simplify_float = identity

let simplify_list = List.map
let simplify_option = Util.Option.map

let simplify_tuple2 f1 f2 (x1, x2) =
  (f1 x1, f2 x2)

let simplify_tuple3 f1 f2 f3 (x1, x2, x3) =
  (f1 x1, f2 x2, f3 x3)
