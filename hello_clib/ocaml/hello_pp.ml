(* Hello pretty printer *)

open Hello_ast

let rec pp_stmt ff (s : stmt) =
	match s with
	| Skip -> Format.fprintf ff "Skip"
	| Print e -> Format.fprintf ff "Print %a" pp_expr e
	| Block ss -> Formatx.pp_list ~sep:(Formatx.pp_sep ";") pp_stmt ff ss
	
and pp_expr ff (e : expr) =
	match e with
	| Unit -> Format.fprintf ff "Unit"
	| IntConst i -> Format.fprintf ff "%d" i
	| BinaryOp(op, e1, e2) -> Format.fprintf ff "(%a %a %a)" pp_expr e1 pp_binary_op op pp_expr e2
	| ConstDecl(name, e1, e2) -> Format.fprintf ff "Let %s = %a in \n %a" name pp_expr e1 pp_expr e2
	| Var name -> Format.fprintf ff "%s" name

and pp_binary_op ff (op : binary_op) =
	match op with
	| BinaryOp_Add -> Format.fprintf ff "+"
	| BinaryOp_Multiply -> Format.fprintf ff "*"
