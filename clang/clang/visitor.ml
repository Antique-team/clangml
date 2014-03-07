open Ast

type 'a visitor = {
  map_desg : 'a -> desg -> 'a * desg;
  map_decl : 'a -> decl -> 'a * decl;
  map_expr : 'a -> expr -> 'a * expr;
  map_stmt : 'a -> stmt -> 'a * stmt;
  map_ctyp : 'a -> ctyp -> 'a * ctyp;
  map_tloc : 'a -> tloc -> 'a * tloc;
}


let map_opt v state = function
  | Some value ->
      let (state, value) = v state value in
      (state, Some value)
  | None ->
      (state, None)


let map_list v state xs =
  let (state, xs) =
    List.fold_left (fun (state, xs) x ->
      let (state, x) = v state x in
      (state, x :: xs)
    ) (state, []) xs
  in
  (state, List.rev xs)


let visit_desg v state desg =
  let (state, dr) =
    match desg.dr with
    | FieldDesignator name ->
        (state, FieldDesignator name)
    | ArrayDesignator index ->
        let (state, index) = v.map_expr state index in
        (state, ArrayDesignator index)
    | ArrayRangeDesignator (range_start, range_end) ->
        let (state, range_start) = v.map_expr state range_start in
        let (state, range_end) = v.map_expr state range_end in
        (state, ArrayRangeDesignator (range_start, range_end))
  in
  (state, { desg with dr })


let visit_decl v state decl =
  let (state, d) =
    match decl.d with
    | UnimpDecl name ->
        failwith name

    | EmptyDecl ->
        (state, EmptyDecl)
    | TranslationUnitDecl decls ->
        let (state, decls) = map_list v.map_decl state decls in
        (state, TranslationUnitDecl decls)
    | FunctionDecl (ty, name, body) ->
        let (state, ty) = v.map_tloc state ty in
        let (state, body) = map_opt v.map_stmt state body in
        (state, FunctionDecl (ty, name, body))
    | TypedefDecl (ty, name) ->
        let (state, ty) = v.map_tloc state ty in
        (state, TypedefDecl (ty, name))
    | VarDecl (ty, name, init) ->
        let (state, ty) = v.map_tloc state ty in
        let (state, init) = map_opt v.map_expr state init in
        (state, VarDecl (ty, name, init))
    | ParmVarDecl (ty, name) ->
        let (state, ty) = v.map_tloc state ty in
        (state, ParmVarDecl (ty, name))
    | RecordDecl (name, members) ->
        let (state, members) = map_list v.map_decl state members in
        (state, RecordDecl (name, members))
    | FieldDecl (ty, name, bitwidth, init) ->
        let (state, ty) = v.map_tloc state ty in
        let (state, bitwidth) = map_opt v.map_expr state bitwidth in
        let (state, init) = map_opt v.map_expr state init in
        (state, FieldDecl (ty, name, bitwidth, init))
    | EnumDecl (name, enums) ->
        let (state, enums) = map_list v.map_decl state enums in
        (state, EnumDecl (name, enums))
    | EnumConstantDecl (name, value) ->
        let (state, value) = map_opt v.map_expr state value in
        (state, EnumConstantDecl (name, value))
  in
  (state, { decl with d })


let visit_expr v state expr =
  let (state, e) =
    match expr.e with
    | UnimpExpr name ->
        failwith name

    | IntegerLiteral i ->
        (state, IntegerLiteral i)
    | CharacterLiteral c ->
        (state, CharacterLiteral c)
    | FloatingLiteral f ->
        (state, FloatingLiteral f)
    | StringLiteral s ->
        (state, StringLiteral s)
    | BinaryOperator (op, lhs, rhs) ->
        let (state, lhs) = v.map_expr state lhs in
        let (state, rhs) = v.map_expr state rhs in
        (state, BinaryOperator (op, lhs, rhs))
    | UnaryOperator (op, operand) ->
        let (state, operand) = v.map_expr state operand in
        (state, UnaryOperator (op, operand))

    | DeclRefExpr name ->
        (state, DeclRefExpr name)
    | PredefinedExpr kind ->
        (state, PredefinedExpr kind)
    | ImplicitCastExpr (kind, expr) ->
        let (state, expr) = v.map_expr state expr in
        (state, ImplicitCastExpr (kind, expr))
    | CStyleCastExpr (kind, ty, expr) ->
        let (state, ty) = v.map_tloc state ty in
        let (state, expr) = v.map_expr state expr in
        (state, CStyleCastExpr (kind, ty, expr))
    | CompoundLiteralExpr (ty, init) ->
        let (state, ty) = v.map_tloc state ty in
        let (state, init) = v.map_expr state init in
        (state, CompoundLiteralExpr (ty, init))
    | ParenExpr expr ->
        let (state, expr) = v.map_expr state expr in
        (state, ParenExpr expr)
    | VAArgExpr (expr, ty) ->
        let (state, expr) = v.map_expr state expr in
        let (state, ty) = v.map_tloc state ty in
        (state, VAArgExpr (expr, ty))
    | CallExpr (callee, args) ->
        let (state, callee) = v.map_expr state callee in
        let (state, args) = map_list v.map_expr state args in
        (state, CallExpr (callee, args))
    | MemberExpr (base, member, is_arrow) ->
        let (state, base) = v.map_expr state base in
        (state, MemberExpr (base, member, is_arrow))
    | ConditionalOperator (cond, then_expr, else_expr) ->
        let (state, cond) = v.map_expr state cond in
        let (state, then_expr) = v.map_expr state then_expr in
        let (state, else_expr) = v.map_expr state else_expr in
        (state, ConditionalOperator (cond, then_expr, else_expr))
    | DesignatedInitExpr (desgs, init) ->
        let (state, desgs) = map_list v.map_desg state desgs in
        let (state, init) = v.map_expr state init in
        (state, DesignatedInitExpr (desgs, init))
    | InitListExpr inits ->
        let (state, inits) = map_list v.map_expr state inits in
        (state, InitListExpr inits)
    | ImplicitValueInitExpr ->
        (state, ImplicitValueInitExpr)
    | ArraySubscriptExpr (base, index) ->
        let (state, base) = v.map_expr state base in
        let (state, index) = v.map_expr state index in
        (state, ArraySubscriptExpr (base, index))
    | StmtExpr stmt ->
        let (state, stmt) = v.map_stmt state stmt in
        (state, StmtExpr stmt)

    | SizeOfExpr expr ->
        let (state, expr) = v.map_expr state expr in
        (state, SizeOfExpr expr)
    | SizeOfType ty ->
        let (state, ty) = v.map_tloc state ty in
        (state, SizeOfType ty)
    | AlignOfExpr expr ->
        let (state, expr) = v.map_expr state expr in
        (state, AlignOfExpr expr)
    | AlignOfType ty ->
        let (state, ty) = v.map_tloc state ty in
        (state, AlignOfType ty)
    | VecStepExpr expr ->
        let (state, expr) = v.map_expr state expr in
        (state, VecStepExpr expr)
    | VecStepType ty ->
        let (state, ty) = v.map_tloc state ty in
        (state, VecStepType ty)
  in
  (state, { expr with e })


let visit_stmt v state stmt =
  let (state, s) =
    match stmt.s with
    | UnimpStmt name ->
        failwith name

    | NullStmt ->
        (state, NullStmt)
    | BreakStmt ->
        (state, BreakStmt)
    | ContinueStmt ->
        (state, ContinueStmt)
    | LabelStmt (label, stmt) ->
        let (state, stmt) = v.map_stmt state stmt in
        (state, LabelStmt (label, stmt))
    | CaseStmt (range_start, range_end, stmt) ->
        let (state, range_start) = v.map_expr state range_start in
        let (state, range_end) = map_opt v.map_expr state range_end in
        let (state, stmt) = v.map_stmt state stmt in
        (state, CaseStmt (range_start, range_end, stmt))
    | DefaultStmt stmt ->
        let (state, stmt) = v.map_stmt state stmt in
        (state, DefaultStmt stmt)
    | GotoStmt label ->
        (state, GotoStmt label)
    | ExprStmt expr ->
        let (state, expr) = v.map_expr state expr in
        (state, ExprStmt expr)
    | CompoundStmt body ->
        let (state, body) = map_list v.map_stmt state body in
        (state, CompoundStmt body)
    | ReturnStmt expr ->
        let (state, expr) = map_opt v.map_expr state expr in
        (state, ReturnStmt expr)
    | IfStmt (cond, then_stmt, else_stmt) ->
        let (state, cond) = v.map_expr state cond in
        let (state, then_stmt) = v.map_stmt state then_stmt in
        let (state, else_stmt) = map_opt v.map_stmt state else_stmt in
        (state, IfStmt (cond, then_stmt, else_stmt))
    | ForStmt (init, cond, incr, body) ->
        let (state, init) = map_opt v.map_stmt state init in
        let (state, cond) = map_opt v.map_expr state cond in
        let (state, incr) = map_opt v.map_expr state incr in
        let (state, body) = v.map_stmt state body in
        (state, ForStmt (init, cond, incr, body))
    | WhileStmt (cond, body) ->
        let (state, cond) = v.map_expr state cond in
        let (state, body) = v.map_stmt state body in
        (state, WhileStmt (cond, body))
    | DoStmt (body, cond) ->
        let (state, body) = v.map_stmt state body in
        let (state, cond) = v.map_expr state cond in
        (state, DoStmt (body, cond))
    | SwitchStmt (expr, body) ->
        let (state, expr) = v.map_expr state expr in
        let (state, body) = v.map_stmt state body in
        (state, SwitchStmt (expr, body))
    | DeclStmt decls ->
        let (state, decls) = map_list v.map_decl state decls in
        (state, DeclStmt decls)
  in
  (state, { stmt with s })


let visit_ctyp v state ctyp =
  let (state, t) =
    match ctyp.t with
    | UnimpType name ->
        failwith name

    | BuiltinType bt ->
        (state, BuiltinType bt)
    | TypeOfExprType expr ->
        let (state, expr) = v.map_expr state expr in
        (state, TypeOfExprType expr)
    | TypeOfType ty ->
        let (state, ty) = v.map_ctyp state ty in
        (state, TypeOfType ty)
    | ParenType ty ->
        let (state, ty) = v.map_ctyp state ty in
        (state, ParenType ty)
    | TypedefType name ->
        (state, TypedefType name)
    | PointerType pointee ->
        let (state, pointee) = v.map_ctyp state pointee in
        (state, PointerType pointee)
    | FunctionNoProtoType result ->
        let (state, result) = v.map_ctyp state result in
        (state, FunctionNoProtoType result)
    | FunctionProtoType (result, args) ->
        let (state, result) = v.map_ctyp state result in
        let (state, args) = map_list v.map_ctyp state args in
        (state, FunctionProtoType (result, args))
    | ConstantArrayType (memty, size) ->
        let (state, memty) = v.map_ctyp state memty in
        (state, ConstantArrayType (memty, size))
    | VariableArrayType (memty, size) ->
        let (state, memty) = v.map_ctyp state memty in
        let (state, size) = v.map_expr state size in
        (state, VariableArrayType (memty, size))
    | IncompleteArrayType memty ->
        let (state, memty) = v.map_ctyp state memty in
        (state, IncompleteArrayType memty)
    | ElaboratedType ty ->
        let (state, ty) = v.map_ctyp state ty in
        (state, ElaboratedType ty)
    | EnumType name ->
        (state, EnumType name)
    | RecordType (kind, name) ->
        (state, RecordType (kind, name))
    | DecayedType (decayed, original) ->
        let (state, decayed) = v.map_ctyp state decayed in
        let (state, original) = v.map_ctyp state original in
        (state, DecayedType (decayed, original))
  in
  (state, { ctyp with t })


let visit_tloc v state tloc =
  let (state, tl) =
    match tloc.tl with
    | UnimpTypeLoc name ->
        failwith name

    | BuiltinTypeLoc bt ->
        (state, BuiltinTypeLoc bt)
    | TypeOfExprTypeLoc expr ->
        let (state, expr) = v.map_expr state expr in
        (state, TypeOfExprTypeLoc expr)
    | TypeOfTypeLoc ty ->
        let (state, ty) = v.map_tloc state ty in
        (state, TypeOfTypeLoc ty)
    | ParenTypeLoc ty ->
        let (state, ty) = v.map_tloc state ty in
        (state, ParenTypeLoc ty)
    | QualifiedTypeLoc (ty, qual, aspace) ->
        let (state, ty) = v.map_tloc state ty in
        (state, QualifiedTypeLoc (ty, qual, aspace))
    | TypedefTypeLoc name ->
        (state, TypedefTypeLoc name)
    | PointerTypeLoc pointee ->
        let (state, pointee) = v.map_tloc state pointee in
        (state, PointerTypeLoc pointee)
    | FunctionNoProtoTypeLoc result ->
        let (state, result) = v.map_tloc state result in
        (state, FunctionNoProtoTypeLoc result)
    | FunctionProtoTypeLoc (result, args) ->
        let (state, result) = v.map_tloc state result in
        let (state, args) = map_list v.map_decl state args in
        (state, FunctionProtoTypeLoc (result, args))
    | ConstantArrayTypeLoc (memty, size) ->
        let (state, memty) = v.map_tloc state memty in
        (state, ConstantArrayTypeLoc (memty, size))
    | VariableArrayTypeLoc (memty, size) ->
        let (state, memty) = v.map_tloc state memty in
        let (state, size) = v.map_expr state size in
        (state, VariableArrayTypeLoc (memty, size))
    | IncompleteArrayTypeLoc memty ->
        let (state, memty) = v.map_tloc state memty in
        (state, IncompleteArrayTypeLoc memty)
    | ElaboratedTypeLoc ty ->
        let (state, ty) = v.map_tloc state ty in
        (state, ElaboratedTypeLoc ty)
    | EnumTypeLoc name ->
        (state, EnumTypeLoc name)
    | RecordTypeLoc (kind, name) ->
        (state, RecordTypeLoc (kind, name))
  in
  (state, { tloc with tl })


let () =
  (* Default visitors that simply perform a deep copy
     of the data structure. *)
  let rec v = {
    map_desg = (fun state desg -> visit_desg v state desg);
    map_decl = (fun state desg -> visit_decl v state desg);
    map_expr = (fun state desg -> visit_expr v state desg);
    map_ctyp = (fun state desg -> visit_ctyp v state desg);
    map_tloc = (fun state desg -> visit_tloc v state desg);
    map_stmt = (fun state desg -> visit_stmt v state desg);
  } in ignore v
