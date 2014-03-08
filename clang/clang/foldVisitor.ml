open Ast

type 'a visitor = {
  fold_desg : 'a visitor -> 'a -> desg -> 'a;
  fold_decl : 'a visitor -> 'a -> decl -> 'a;
  fold_expr : 'a visitor -> 'a -> expr -> 'a;
  fold_stmt : 'a visitor -> 'a -> stmt -> 'a;
  fold_ctyp : 'a visitor -> 'a -> ctyp -> 'a;
  fold_tloc : 'a visitor -> 'a -> tloc -> 'a;
}


let fold_option v self state = function
  | Some value ->
      v self state value
  | None ->
      state


let fold_list v self =
  List.fold_left (v self)


let visit_desg v state desg =
  match desg.dr with
  | FieldDesignator name ->
      state
  | ArrayDesignator index ->
      let state = v.fold_expr v state index in
      state
  | ArrayRangeDesignator (range_start, range_end) ->
      let state = v.fold_expr v state range_start in
      let state = v.fold_expr v state range_end in
      state


let visit_decl v state decl =
  match decl.d with
  | UnimpDecl name ->
      state

  | EmptyDecl ->
      state
  | TranslationUnitDecl decls ->
      let state = fold_list v.fold_decl v state decls in
      state
  | FunctionDecl (ty, name, body) ->
      let state = v.fold_tloc v state ty in
      let state = fold_option v.fold_stmt v state body in
      state
  | TypedefDecl (ty, name) ->
      let state = v.fold_tloc v state ty in
      state
  | VarDecl (ty, name, init) ->
      let state = v.fold_tloc v state ty in
      let state = fold_option v.fold_expr v state init in
      state
  | ParmVarDecl (ty, name) ->
      let state = v.fold_tloc v state ty in
      state
  | RecordDecl (name, members) ->
      let state = fold_list v.fold_decl v state members in
      state
  | FieldDecl (ty, name, bitwidth, init) ->
      let state = v.fold_tloc v state ty in
      let state = fold_option v.fold_expr v state bitwidth in
      let state = fold_option v.fold_expr v state init in
      state
  | EnumDecl (name, enums) ->
      let state = fold_list v.fold_decl v state enums in
      state
  | EnumConstantDecl (name, value) ->
      let state = fold_option v.fold_expr v state value in
      state


let visit_expr v state expr =
  match expr.e with
  | UnimpExpr name ->
      state

  | IntegerLiteral i ->
      state
  | CharacterLiteral c ->
      state
  | FloatingLiteral f ->
      state
  | StringLiteral s ->
      state
  | BinaryOperator (op, lhs, rhs) ->
      let state = v.fold_expr v state lhs in
      let state = v.fold_expr v state rhs in
      state
  | UnaryOperator (op, operand) ->
      let state = v.fold_expr v state operand in
      state

  | DeclRefExpr name ->
      state
  | PredefinedExpr kind ->
      state
  | ImplicitCastExpr (kind, expr) ->
      let state = v.fold_expr v state expr in
      state
  | CStyleCastExpr (kind, ty, expr) ->
      let state = v.fold_tloc v state ty in
      let state = v.fold_expr v state expr in
      state
  | CompoundLiteralExpr (ty, init) ->
      let state = v.fold_tloc v state ty in
      let state = v.fold_expr v state init in
      state
  | ParenExpr expr ->
      let state = v.fold_expr v state expr in
      state
  | VAArgExpr (expr, ty) ->
      let state = v.fold_expr v state expr in
      let state = v.fold_tloc v state ty in
      state
  | CallExpr (callee, args) ->
      let state = v.fold_expr v state callee in
      let state = fold_list v.fold_expr v state args in
      state
  | MemberExpr (base, member, is_arrow) ->
      let state = v.fold_expr v state base in
      state
  | ConditionalOperator (cond, then_expr, else_expr) ->
      let state = v.fold_expr v state cond in
      let state = v.fold_expr v state then_expr in
      let state = v.fold_expr v state else_expr in
      state
  | DesignatedInitExpr (desgs, init) ->
      let state = fold_list v.fold_desg v state desgs in
      let state = v.fold_expr v state init in
      state
  | InitListExpr inits ->
      let state = fold_list v.fold_expr v state inits in
      state
  | ImplicitValueInitExpr ->
      state
  | ArraySubscriptExpr (base, index) ->
      let state = v.fold_expr v state base in
      let state = v.fold_expr v state index in
      state
  | StmtExpr stmt ->
      let state = v.fold_stmt v state stmt in
      state

  | SizeOfExpr expr ->
      let state = v.fold_expr v state expr in
      state
  | SizeOfType ty ->
      let state = v.fold_tloc v state ty in
      state
  | AlignOfExpr expr ->
      let state = v.fold_expr v state expr in
      state
  | AlignOfType ty ->
      let state = v.fold_tloc v state ty in
      state
  | VecStepExpr expr ->
      let state = v.fold_expr v state expr in
      state
  | VecStepType ty ->
      let state = v.fold_tloc v state ty in
      state


let visit_stmt v state stmt =
  match stmt.s with
  | UnimpStmt name ->
      state

  | NullStmt ->
      state
  | BreakStmt ->
      state
  | ContinueStmt ->
      state
  | LabelStmt (label, stmt) ->
      let state = v.fold_stmt v state stmt in
      state
  | CaseStmt (range_start, range_end, stmt) ->
      let state = v.fold_expr v state range_start in
      let state = fold_option v.fold_expr v state range_end in
      let state = v.fold_stmt v state stmt in
      state
  | DefaultStmt stmt ->
      let state = v.fold_stmt v state stmt in
      state
  | GotoStmt label ->
      state
  | ExprStmt expr ->
      let state = v.fold_expr v state expr in
      state
  | CompoundStmt body ->
      let state = fold_list v.fold_stmt v state body in
      state
  | ReturnStmt expr ->
      let state = fold_option v.fold_expr v state expr in
      state
  | IfStmt (cond, then_stmt, else_stmt) ->
      let state = v.fold_expr v state cond in
      let state = v.fold_stmt v state then_stmt in
      let state = fold_option v.fold_stmt v state else_stmt in
      state
  | ForStmt (init, cond, incr, body) ->
      let state = fold_option v.fold_stmt v state init in
      let state = fold_option v.fold_expr v state cond in
      let state = fold_option v.fold_expr v state incr in
      let state = v.fold_stmt v state body in
      state
  | WhileStmt (cond, body) ->
      let state = v.fold_expr v state cond in
      let state = v.fold_stmt v state body in
      state
  | DoStmt (body, cond) ->
      let state = v.fold_stmt v state body in
      let state = v.fold_expr v state cond in
      state
  | SwitchStmt (expr, body) ->
      let state = v.fold_expr v state expr in
      let state = v.fold_stmt v state body in
      state
  | DeclStmt decls ->
      let state = fold_list v.fold_decl v state decls in
      state


let visit_ctyp v state ctyp =
  match ctyp.t with
  | UnimpType name ->
      state

  | BuiltinType bt ->
      state
  | TypeOfExprType expr ->
      let state = v.fold_expr v state expr in
      state
  | TypeOfType ty ->
      let state = v.fold_ctyp v state ty in
      state
  | ParenType ty ->
      let state = v.fold_ctyp v state ty in
      state
  | TypedefType name ->
      state
  | PointerType pointee ->
      let state = v.fold_ctyp v state pointee in
      state
  | FunctionNoProtoType result ->
      let state = v.fold_ctyp v state result in
      state
  | FunctionProtoType (result, args) ->
      let state = v.fold_ctyp v state result in
      let state = fold_list v.fold_ctyp v state args in
      state
  | ConstantArrayType (memty, size) ->
      let state = v.fold_ctyp v state memty in
      state
  | VariableArrayType (memty, size) ->
      let state = v.fold_ctyp v state memty in
      let state = v.fold_expr v state size in
      state
  | IncompleteArrayType memty ->
      let state = v.fold_ctyp v state memty in
      state
  | ElaboratedType ty ->
      let state = v.fold_ctyp v state ty in
      state
  | EnumType name ->
      state
  | RecordType (kind, name) ->
      state
  | DecayedType (decayed, original) ->
      let state = v.fold_ctyp v state decayed in
      let state = v.fold_ctyp v state original in
      state


let visit_tloc v state tloc =
  match tloc.tl with
  | UnimpTypeLoc name ->
      state

  | BuiltinTypeLoc bt ->
      state
  | TypeOfExprTypeLoc expr ->
      let state = v.fold_expr v state expr in
      state
  | TypeOfTypeLoc ty ->
      let state = v.fold_tloc v state ty in
      state
  | ParenTypeLoc ty ->
      let state = v.fold_tloc v state ty in
      state
  | QualifiedTypeLoc (ty, qual, aspace) ->
      let state = v.fold_tloc v state ty in
      state
  | TypedefTypeLoc name ->
      state
  | PointerTypeLoc pointee ->
      let state = v.fold_tloc v state pointee in
      state
  | FunctionNoProtoTypeLoc result ->
      let state = v.fold_tloc v state result in
      state
  | FunctionProtoTypeLoc (result, args) ->
      let state = v.fold_tloc v state result in
      let state = fold_list v.fold_decl v state args in
      state
  | ConstantArrayTypeLoc (memty, size) ->
      let state = v.fold_tloc v state memty in
      state
  | VariableArrayTypeLoc (memty, size) ->
      let state = v.fold_tloc v state memty in
      let state = v.fold_expr v state size in
      state
  | IncompleteArrayTypeLoc memty ->
      let state = v.fold_tloc v state memty in
      state
  | ElaboratedTypeLoc ty ->
      let state = v.fold_tloc v state ty in
      state
  | EnumTypeLoc name ->
      state
  | RecordTypeLoc (kind, name) ->
      state


(* Default visitor that simply walks the entire structure. *)
let default = {
  fold_desg = visit_desg;
  fold_decl = visit_decl;
  fold_expr = visit_expr;
  fold_ctyp = visit_ctyp;
  fold_tloc = visit_tloc;
  fold_stmt = visit_stmt;
}
