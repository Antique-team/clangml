open ClangAst

let return_type_of_tloc = function
  | FunctionProtoTypeLoc (_, result, _)
  | FunctionNoProtoTypeLoc (_, result) ->
      result
  | _ -> failwith "return_type_of_tloc: non-function type"


let args_of_tloc = function
  | FunctionProtoTypeLoc (_, _, args) -> args
  | FunctionNoProtoTypeLoc _ -> []
  | _ -> failwith "args_of_tloc: non-function type"


let body_of_stmt = function
  | CompoundStmt (_, stmts) -> stmts
  | stmt -> [stmt]


let rec identifier_of_expr = function
  | DeclRefExpr (_, name) -> name

  | UnimpExpr (_, name) -> failwith ("unimplemented expr: " ^ name)

  | ImplicitCastExpr (_, expr)
  | ParenExpr (_, expr)
  | TypedExpr (expr, _) -> identifier_of_expr expr

  | _ -> failwith "invalid expression (not an identifier)"
