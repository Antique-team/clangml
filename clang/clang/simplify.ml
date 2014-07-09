let identity x = x

let simplify_bool = identity
let simplify_sloc = identity
let simplify_string = identity
let simplify_int = identity
let simplify_char = identity
let simplify_float = identity

let simplify_list = List.map
let simplify_option = Util.Option.map
