let (%) f g = fun x -> f (g x)


let finally f x after =
  (try f x with exn ->
    after ();
    raise exn);
  after ()
