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
