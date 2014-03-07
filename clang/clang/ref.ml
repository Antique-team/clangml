(* A reference to a clang AST node object. This can be freely
   passed over the client/server communication channel. *)
type 'a t = private int

module Show_t(S : Deriving_Show.Show) =
  Deriving_Show.Defaults(struct

    type a = S.a t

    let format fmt (a : a) =
      Format.fprintf fmt "<%d>" (a :> int)

  end)
