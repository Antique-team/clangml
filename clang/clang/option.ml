let map f = function
  | None -> None
  | Some v -> Some (f v)


let default value = function
  | None -> value
  | Some v -> v


let to_list = function
  | None -> []
  | Some v -> [v]
