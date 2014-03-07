type 'a t = int

let null = 0
let is_null ref = ref = 0

module Show_t(S : Deriving_Show.Show) =
  Deriving_Show.Defaults(struct

    type a = S.a t

    let format fmt (a : a) =
      Format.fprintf fmt "<%d>" a

  end)
