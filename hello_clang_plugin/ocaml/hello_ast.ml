type unary_op =
  | UO_PostInc	(* [C99 6.5.2.4] Postfix increment and decrement *)
  | UO_PostDec
  | UO_PreInc	(* [C99 6.5.3.1] Prefix increment and decrement *)
  | UO_PreDec
  | UO_AddrOf	(* [C99 6.5.3.2] Address and indirection *)
  | UO_Deref
  | UO_Plus	(* [C99 6.5.3.3] Unary arithmetic *)
  | UO_Minus
  | UO_Not
  | UO_LNot
  | UO_Real	(* "__real expr"/"__imag expr" Extension. *)
  | UO_Imag
  | UO_Extension(* __extension__ marker. *)


type binary_op =
  | BO_PtrMemD	(* [C++ 5.5] Pointer-to-member operators. *)
  | BO_PtrMemI
  | BO_Mul	(* [C99 6.5.5] Multiplicative operators. *)
  | BO_Div
  | BO_Rem
  | BO_Add	(* [C99 6.5.6] Additive operators. *)
  | BO_Sub
  | BO_Shl	(* [C99 6.5.7] Bitwise shift operators. *)
  | BO_Shr
  | BO_LT	(* [C99 6.5.8] Relational operators. *)
  | BO_GT
  | BO_LE
  | BO_GE
  | BO_EQ	(* [C99 6.5.9] Equality operators. *)
  | BO_NE
  | BO_And	(* [C99 6.5.10] Bitwise AND operator. *)
  | BO_Xor	(* [C99 6.5.11] Bitwise XOR operator. *)
  | BO_Or	(* [C99 6.5.12] Bitwise OR operator. *)
  | BO_LAnd	(* [C99 6.5.13] Logical AND operator. *)
  | BO_LOr	(* [C99 6.5.14] Logical OR operator. *)
  | BO_Assign	(* [C99 6.5.16] Assignment operators. *)
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
  | BO_Comma	(* [C99 6.5.17] Comma operator. *)


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
