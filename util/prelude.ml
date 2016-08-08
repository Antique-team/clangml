let (%) f g = fun x -> f (g x)


let finally f after =
  let result =
    try f () with exn ->
      after ();
      raise exn
  in
  after ();
  result


external identity : 'a -> 'a = "%identity"


let abort (log_stmt: unit) =
  exit 1


let string_of_list to_str sep l' =
  let l = List.rev l' in
  "[" ^ String.concat sep (List.rev_map to_str l) ^ "]"
