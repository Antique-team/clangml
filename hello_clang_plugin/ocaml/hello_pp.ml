(* Hello pretty printer *)

open Hello_ast


let is_prefix = function
  | UO_PostInc
  | UO_PostDec -> false
  | _ -> true


let pp_unary_op ff = function
  | UO_PostInc		-> Format.pp_print_string ff "++"
  | UO_PostDec		-> Format.pp_print_string ff "--"
  | UO_PreInc		-> Format.pp_print_string ff "++"
  | UO_PreDec		-> Format.pp_print_string ff "--"
  | UO_AddrOf		-> Format.pp_print_string ff "&"
  | UO_Deref		-> Format.pp_print_string ff "*"
  | UO_Plus		-> Format.pp_print_string ff "+"
  | UO_Minus		-> Format.pp_print_string ff "-"
  | UO_Not		-> Format.pp_print_string ff "~"
  | UO_LNot		-> Format.pp_print_string ff "!"
  | UO_Real		-> Format.pp_print_string ff "__real"
  | UO_Imag		-> Format.pp_print_string ff "__imag"
  | UO_Extension	-> Format.pp_print_string ff "__extension__"


let pp_binary_op ff = function
  | BO_PtrMemD		-> Format.pp_print_string ff "->*"
  | BO_PtrMemI		-> Format.pp_print_string ff "->*"
  | BO_Mul		-> Format.pp_print_string ff "*"
  | BO_Div		-> Format.pp_print_string ff "/"
  | BO_Rem		-> Format.pp_print_string ff "%"
  | BO_Add		-> Format.pp_print_string ff "+"
  | BO_Sub		-> Format.pp_print_string ff "-"
  | BO_Shl		-> Format.pp_print_string ff "<<"
  | BO_Shr		-> Format.pp_print_string ff ">>"
  | BO_LT		-> Format.pp_print_string ff "<"
  | BO_GT		-> Format.pp_print_string ff ">"
  | BO_LE		-> Format.pp_print_string ff "<="
  | BO_GE		-> Format.pp_print_string ff ">="
  | BO_EQ		-> Format.pp_print_string ff "=="
  | BO_NE		-> Format.pp_print_string ff "!="
  | BO_And		-> Format.pp_print_string ff "&"
  | BO_Xor		-> Format.pp_print_string ff "^"
  | BO_Or		-> Format.pp_print_string ff "|"
  | BO_LAnd		-> Format.pp_print_string ff "&&"
  | BO_LOr		-> Format.pp_print_string ff "||"
  | BO_Assign		-> Format.pp_print_string ff "="
  | BO_Comma		-> Format.pp_print_string ff ","

  | BO_MulAssign	-> Format.pp_print_string ff "*="
  | BO_DivAssign	-> Format.pp_print_string ff "/="
  | BO_RemAssign	-> Format.pp_print_string ff "%="
  | BO_AddAssign	-> Format.pp_print_string ff "+="
  | BO_SubAssign	-> Format.pp_print_string ff "-="
  | BO_ShlAssign	-> Format.pp_print_string ff "<<="
  | BO_ShrAssign	-> Format.pp_print_string ff ">>="
  | BO_AndAssign	-> Format.pp_print_string ff "&="
  | BO_OrAssign		-> Format.pp_print_string ff "|="
  | BO_XorAssign	-> Format.pp_print_string ff "^*"


let rec pp_stmt ff = function
  | Skip -> Format.pp_print_string ff "Skip"
  | Print e -> Format.fprintf ff "Print %a" pp_expr e
  | Block ss -> Formatx.pp_list ~sep:(Formatx.pp_sep ";") pp_stmt ff ss


and pp_expr ff = function
  | Unit -> Format.pp_print_string ff "Unit"

  | CharacterLiteral c -> Format.pp_print_string ff (Char.escaped c)
  | IntegerLiteral i -> Format.pp_print_int ff i
  | FloatingLiteral f -> Format.pp_print_float ff f
  | StringLiteral s -> Format.fprintf ff "\"%s\"" (String.escaped s)
  | UnaryOperator (op, e) ->
      if is_prefix op then
        Format.fprintf ff "(%a %a)" pp_unary_op op pp_expr e
      else
        Format.fprintf ff "(%a %a)" pp_expr e pp_unary_op op
  | BinaryOperator (op, e1, e2) -> Format.fprintf ff "(%a %a %a)" pp_expr e1 pp_binary_op op pp_expr e2
