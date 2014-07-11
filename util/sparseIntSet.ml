module Make(S : sig type t = private int end) : Set.S
  with type elt = S.t =
struct

  include Set.Make(struct
    include S

    let compare : t -> t -> int = Pervasives.compare
  end)

end
