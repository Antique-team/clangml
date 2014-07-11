open Prelude


type 'key key = int

let null_key = -1

module Show_key(S : Deriving_Show.Show) =
  Deriving_Show.Defaults(struct

    type a = S.a key

    let format fmt (a : a) =
      Format.fprintf fmt "<%d>" a

  end)


type ('key, 'value) t = 'value array


let find idx arr =
  Array.get arr (idx : 'key key :> int)


let iter = Array.iteri


let fold f arr x =
  snd @@ Array.fold_left (fun (i, x) value ->
    (i + 1, f i value x)
  ) (0, x) arr


let map f arr =
  let mapped = Array.mapi f arr in
  (Array.init (Array.length arr) identity, mapped)


let mapv = Array.mapi


let mapvf f arr state =
  let (state, _, mapped) =
    Array.fold_left
      (fun (state, i, mapped) elt ->
         let (state, mapped_elt) = f i elt state in
         let mapped = mapped_elt :: mapped in
         (state, i + 1, mapped)
      )
      (state, 0, [])
      arr
  in

  state, Array.of_list (List.rev mapped)




let cardinal = Array.length
let exists f arr =
  Array.fold_left (fun result elt ->
    result || f elt
  ) false arr
