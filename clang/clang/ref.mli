(* A reference to a clang AST node object. This can be freely
   passed over the client/server communication channel. *)
type 'a t

val null : 'a t
val is_null : 'a t -> bool

module Show_t :
  functor (S : Deriving_Show.Show) ->
    Deriving_Show.Show
      with type a = S.a t
