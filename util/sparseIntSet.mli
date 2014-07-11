module Make(S : sig type t = private int end) : Set.S
  with type elt = S.t
