let map f = function
  | None -> None
  | Some v -> Some (f v)
