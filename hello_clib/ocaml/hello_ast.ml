
(* Changing the ordering of variants requires changing the C interface -- so don't. *)

type binary_op =
	| BinaryOp_Add
	| BinaryOp_Multiply


type expr =
	| Unit
	| IntConst of int
	| BinaryOp of binary_op * expr * expr
	| ConstDecl of string * expr * expr
	| Var of string

type stmt =
	| Skip
	| Print of expr
	| Block of stmt list
