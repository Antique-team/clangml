type unary_op =
  | UO_PostInc
  | UO_PostDec
  | UO_PreInc
  | UO_PreDec
  | UO_AddrOf
  | UO_Deref
  | UO_Plus
  | UO_Minus
  | UO_Not
  | UO_LNot
  | UO_Real
  | UO_Imag
  | UO_Extension


type binary_op =
  | BO_PtrMemD
  | BO_PtrMemI
  | BO_Mul
  | BO_Div
  | BO_Rem
  | BO_Add
  | BO_Sub
  | BO_Shl
  | BO_Shr
  | BO_LT
  | BO_GT
  | BO_LE
  | BO_GE
  | BO_EQ
  | BO_NE
  | BO_And
  | BO_Xor
  | BO_Or
  | BO_LAnd
  | BO_LOr
  | BO_Assign
  | BO_Comma

  | BO_MulAssign
  | BO_DivAssign
  | BO_RemAssign
  | BO_AddAssign
  | BO_SubAssign
  | BO_ShlAssign
  | BO_ShrAssign
  | BO_AndAssign
  | BO_OrAssign
  | BO_XorAssign


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
  | IntegerLiteral	of int
  | CharacterLiteral	of char
  | FloatingLiteral	of float
  | StringLiteral	of string
  | BinaryOperator	of binary_op * expr * expr
  | UnaryOperator	of unary_op * expr

and stmt =
  | Skip
  | Print	of expr
  | Block	of stmt list
