module Make(S : sig type t = private int end) : Map.S
  with type key = S.t
