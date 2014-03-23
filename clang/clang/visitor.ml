let iter_option v self = function
  | Some value ->
      v self value
  | None ->
      ()


let iter_list v self =
  List.iter (v self)



let fold_option v self state = function
  | Some value ->
      v self state value
  | None ->
      state


let fold_list v self =
  List.fold_left (v self)



let map_option v self state = function
  | Some value ->
      let (state, value) = v self state value in
      (state, Some value)
  | None ->
      (state, None)


let map_list v self state xs =
  let (state, xs) =
    List.fold_left (fun (state, xs) x ->
      let (state, x) = v self state x in
      (state, x :: xs)
    ) (state, []) xs
  in
  (state, List.rev xs)
