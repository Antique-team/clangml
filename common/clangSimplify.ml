open ClangAst


let rec simplify_local_decl_ result = function
  | VarDecl (ty, name, init) ->
      VarDecl (ty, name, init)
      :: result

  | d -> failwith (Show.show<decl_> d)


and simplify_local_decls decls =
  let rec simplify result = function
    | [] -> List.rev result
    | { d } as decl :: decls ->
        simplify (
          List.rev_map (fun d -> { decl with d })
            (simplify_local_decl_ [] d)
          @ result
        ) decls
  in
  simplify [] decls


and simplify_stmt_ result = function
  | DeclStmt decls ->
      List.map (fun d -> DeclStmt [d])
        (simplify_local_decls decls)
      @ result

  | ExprStmt expr ->
      ExprStmt expr
      :: result

  | s -> failwith (Show.show<stmt_> s);


and simplify_stmts stmts =
  let rec simplify result = function
    | [] -> List.rev result
    | { s } as stmt :: stmts ->
        simplify (
          List.rev_map (fun s -> { stmt with s })
            (simplify_stmt_ [] s)
          @ result
        ) stmts
  in
  simplify [] stmts


and simplify_decl_ result = function
  (* We could keep them, but for simplification purposes, we just
     throw them away. *)
  | UnimpDecl _
  | EmptyDecl -> result

  | FunctionDecl (ty, name, Some ({ s = CompoundStmt stmts } as body)) ->
      FunctionDecl (ty, name, Some { body with s = CompoundStmt (simplify_stmts stmts) })
      :: result

  | FunctionDecl (ty, name, None) ->
      FunctionDecl (ty, name, None)
      :: result

  | TypedefDecl (ty, name) ->
      TypedefDecl (ty, name)
      :: result

  | VarDecl (ty, name, init) ->
      VarDecl  (ty, name, init)
      :: result

  | RecordDecl (name, members) ->
      RecordDecl (name, members)
      :: result

  | EnumDecl (name, enums) ->
      EnumDecl (name, enums)
      :: result

  | FunctionDecl _ (* with non-CompoundStmt body *)
  | ParmVarDecl _
  | FieldDecl _
  | EnumConstantDecl _
  | TranslationUnitDecl _ ->
      assert false


and simplify_decls decls =
  let rec simplify result = function
    | [] -> List.rev result
    | { d } as decl :: decls ->
        simplify (
          List.rev_map (fun d -> { decl with d })
            (simplify_decl_ [] d)
          @ result
        ) decls
  in
  simplify [] decls


let simplify_unit = function
  | { d = TranslationUnitDecl decls } as decl ->
      { decl with d = TranslationUnitDecl (simplify_decls decls) }
  | _ -> failwith "simplify_unit needs a TranslationUnitDecl"
