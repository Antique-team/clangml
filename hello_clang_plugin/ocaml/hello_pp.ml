(* Hello pretty printer *)

open Hello_ast

let rec pp_stmt ff = function
  | Skip -> Format.fprintf ff "Skip"
  | Print e -> Format.fprintf ff "Print %a" pp_expr e
  | Block ss -> Formatx.pp_list ~sep:(Formatx.pp_sep ";") pp_stmt ff ss

and pp_expr ff = function
  | Unit -> Format.fprintf ff "Unit"
  | CharConst c -> Format.fprintf ff "%s" (Char.escaped c)
  | IntConst i -> Format.fprintf ff "%d" i
  | FloatConst f -> Format.fprintf ff "%f" f
  | StringConst s -> Format.fprintf ff "\"%s\"" (String.escaped s)
  | BinaryOp (op, e1, e2) -> Format.fprintf ff "(%a %a %a)" pp_expr e1 pp_binary_op op pp_expr e2

and pp_binary_op ff = function
  | BinaryOp_Add -> Format.fprintf ff "+"
  | BinaryOp_Multiply -> Format.fprintf ff "*"
