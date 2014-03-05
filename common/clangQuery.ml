open ClangAst

let return_type_of_tloc = function
  | FunctionProtoTypeLoc (result, _)
  | FunctionNoProtoTypeLoc (result) ->
      result
  | _ -> failwith "return_type_of_tloc: non-function type"


let args_of_tloc = function
  | FunctionProtoTypeLoc (_, args) -> args
  | FunctionNoProtoTypeLoc _ -> []
  | _ -> failwith "args_of_tloc: non-function type"


let body_of_stmt = function
  | { s = CompoundStmt stmts } -> stmts
  | stmt -> [stmt]


let rec identifier_of_expr = function
  | DeclRefExpr (name) -> name

  | UnimpExpr (name) -> failwith ("unimplemented expr: " ^ name)

  | ImplicitCastExpr (expr)
  | ParenExpr (expr) -> identifier_of_expr expr.e

  | _ -> failwith "invalid expression (not an identifier)"
