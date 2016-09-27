open Ast

let return_type_of_tloc = function
  | FunctionProtoTypeLoc (result, _)
  | FunctionNoProtoTypeLoc result ->
      result
  | x -> failwith ("return_type_of_tloc: non-function type: " ^ (Pp.string_of_tloc_ x))

let args_of_tloc = function
  | FunctionProtoTypeLoc (_, args) -> args
  | FunctionNoProtoTypeLoc _ -> []
  | x -> failwith ("args_of_tloc: non-function type: " ^ (Pp.string_of_tloc_ x))


let body_of_stmt = function
  | { s = CompoundStmt stmts } -> stmts
  | stmt -> [stmt]


let rec identifier_of_expr = function
  | DeclRefExpr name -> name

  | ImplicitCastExpr (_, expr)
  | ParenExpr expr -> identifier_of_expr expr.e

  | UnaryOperator (UO_Deref, expr) -> identifier_of_expr expr.e
  | MemberExpr (base, _, _) -> identifier_of_expr base.e
  | ArraySubscriptExpr (base, _index) -> identifier_of_expr base.e

  | e -> failwith
           (Printf.sprintf
              "identifier_of_expr: invalid expression (not an identifier): %s"
              (Pp.string_of_expr_ e))

let is_volatile_tloc = function
  | QualifiedTypeLoc (_, qual, _) ->
      List.memq TQ_Volatile qual

  | _ -> false

let fields_of_record_decl = function
  | RecordDecl (_, _, Some members, _) -> members
  | RecordDecl (ttk, name, None, _) ->
    failwith
      (Printf.sprintf
         "fields_of_record_decl: incomplete record type ttk: %s name: %s"
         (Pp.string_of_tag_type_kind ttk) name)
  | _ -> failwith "decl is not a RecordDecl"

let underlying_type_of_typedef_decl = function
  | TypedefDecl (ty, _) -> ty
  | _ -> failwith "underlying_type_of_typedef_decl: decl is not a TypedefDecl"
