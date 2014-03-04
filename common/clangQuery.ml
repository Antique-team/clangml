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


let body_of_compound_stmt = function
  | CompoundStmt (_, stmts) -> stmts
  | _ -> failwith "body_of_compound_stmt: requires CompoundStmt"
