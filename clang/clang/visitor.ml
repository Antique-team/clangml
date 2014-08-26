let iter_option v self = function
  | Some value ->
      v self value
  | None ->
      ()


let iter_list v self =
  List.iter (v self)


let iter_tuple2 v1 v2 self (x1, x2) =
  v1 self x1;
  v2 self x2;
;;

let iter_tuple3 v1 v2 v3 self (x1, x2, x3) =
  v1 self x1;
  v2 self x2;
  v3 self x3;
;;



let fold_option v self state = function
  | Some value ->
      v self state value
  | None ->
      state


let fold_list v self =
  List.fold_left (v self)


let fold_tuple2 v1 v2 self state (x1, x2) =
  let state = v1 self state x1 in
  let state = v2 self state x2 in
  state

let fold_tuple3 v1 v2 v3 self state (x1, x2, x3) =
  let state = v1 self state x1 in
  let state = v2 self state x2 in
  let state = v3 self state x3 in
  state



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


let map_tuple2 v1 v2 self state (x1, x2) =
  let (state, x1) = v1 self state x1 in
  let (state, x2) = v2 self state x2 in
  (state, (x1, x2))

let map_tuple3 v1 v2 v3 self state (x1, x2, x3) =
  let (state, x1) = v1 self state x1 in
  let (state, x2) = v2 self state x2 in
  let (state, x3) = v3 self state x3 in
  (state, (x1, x2, x3))
