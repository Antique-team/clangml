open Prelude


type 'a key = int

let null_key = -1

module Show_key(S : Deriving_Show.Show) =
  Deriving_Show.Defaults(struct

    type a = S.a key

    let format fmt (a : a) =
      Format.fprintf fmt "<%d>" a

  end)


type 'a t = 'a array


let find idx arr =
  Array.get arr (idx : 'a key :> int)


let iter = Array.iteri


let map f arr =
  let mapped = Array.mapi f arr in
  (Array.init (Array.length arr) identity, mapped)
