type 'a t = int

let null = 0
let is_null ref = ref = 0

let compare a b =
  if is_null a || is_null b then
    failwith "null reference passed to compare";
  Pervasives.compare a b

let hash x =
  if is_null x then
    failwith "null reference passed to hash";
  Hashtbl.hash x

module Show_t(S : Deriving_Show.Show) =
  Deriving_Show.Defaults(struct

    type a = S.a t

    let format fmt (a : a) =
      Format.fprintf fmt "<%d>" a

  end)
