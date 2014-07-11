module Make(S : sig type t = private int end) : Map.S
  with type key = S.t =
struct

  include Map.Make(struct
    include S

    let compare : t -> t -> int = Pervasives.compare
  end)

end
