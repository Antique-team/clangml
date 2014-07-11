open Ast

let return_type_of_tloc = function
  | FunctionProtoTypeLoc (result, _)
  | FunctionNoProtoTypeLoc result ->
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
  | DeclRefExpr name -> name

  | ImplicitCastExpr (_, expr)
  | ParenExpr expr -> identifier_of_expr expr.e

  | _ -> failwith "invalid expression (not an identifier)"


let is_volatile_tloc = function
  | QualifiedTypeLoc (_, qual, _) ->
      List.memq TQ_Volatile qual

  | _ -> false

let fields_of_record_decl = function
  | RecordDecl (_, _, Some members, _) -> members
  | RecordDecl (_, _, None, _) -> failwith "incomplete record type"
  | _ -> failwith "decl is not a RecordDecl"

let underlying_type_of_typedef_decl = function
  | TypedefDecl (ty, _) -> ty
  | _ -> failwith "decl is not a TypedefDecl"
