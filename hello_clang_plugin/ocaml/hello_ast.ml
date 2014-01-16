type binary_op =
  | BinaryOp_Add
  | BinaryOp_Multiply

(*
type expr =
  | Unit of Clang.Unit.t
  | IntConst of Clang.IntConst.t	* int
  | BinaryOp of Clang.BinaryOp.t	* binary_op * expr * expr

and stmt =
  | Skip of Clang.Skip.t
  | Print of Clang.Print.t		* expr
  | Block of Clang.Block.t		* stmt list
*)

type expr =
  | Unit
  | IntConst	of int
  | CharConst	of char
  | FloatConst	of float
  | StringConst	of string
  | BinaryOp	of binary_op * expr * expr

and stmt =
  | Skip
  | Print	of expr
  | Block	of stmt list
