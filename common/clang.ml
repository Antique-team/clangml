type 'a t = private int

module Show_t(S : Deriving_Show.Show) =
  Deriving_Show.Defaults(struct

    type a = S.a t

    let format fmt (a : a) =
      Format.fprintf fmt "<%d>" (a :> int)

  end)
