open Ast

type 'a visitor = {
  decl     : 'a -> decl_     -> decl     -> ('a * decl    );
  expr     : 'a -> expr_     -> expr     -> ('a * expr    );
  stmt     : 'a -> stmt_     -> stmt     -> ('a * stmt    );
  ctyp     : 'a -> ctyp_     -> ctyp     -> ('a * ctyp    );
  type_loc : 'a -> type_loc_ -> type_loc -> ('a * type_loc);
}


let rec default_decl     state decl_     decl     = (state, decl    )
and   default_expr     state expr_     expr     = (state, expr    )
and   default_stmt     state stmt_     stmt     = (state, stmt    )
and   default_ctyp     state ctyp_     ctyp     = (state, ctyp    )
and   default_type_loc state type_loc_ type_loc = (state, type_loc)
and default = {
  decl     = default_decl;
  expr     = default_expr;
  stmt     = default_stmt;
  ctyp     = default_ctyp;
  type_loc = default_type_loc;
}


let see_decl     v state decl     = v.decl     state decl.d      decl
let see_expr     v state expr     = v.expr     state expr.e      expr
let see_stmt     v state stmt     = v.stmt     state stmt.s      stmt
let see_ctyp     v state ctyp     = v.ctyp     state ctyp.t      ctyp
let see_type_loc v state type_loc = v.type_loc state type_loc.tl type_loc


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


let visit_decl v state decl =
  let (state, d) =
    match decl.d with
    | UnimpDecl name ->
        failwith name

    | EmptyDecl ->
        (state, EmptyDecl)

    | TranslationUnitDecl decls ->
        let (state, decls) = map_list (see_decl v) state decls in
        (state, TranslationUnitDecl decls)

    | FunctionDecl (ty, name, body) ->
        let (state, ty) = see_type_loc v state ty in
        let (state, body) = map_opt (see_stmt v) state body in
        (state, FunctionDecl (ty, name, body))

    | TypedefDecl (ty, name) ->
        let (state, ty) = see_type_loc v state ty in
        (state, TypedefDecl (ty, name))

    | VarDecl (ty, name, init) ->
        let (state, ty) = see_type_loc v state ty in
        let (state, init) = map_opt (see_expr v) state init in
        (state, VarDecl (ty, name, init))

    | ParmVarDecl (ty, name) ->
        let (state, ty) = see_type_loc v state ty in
        (state, ParmVarDecl (ty, name))

    | RecordDecl (name, members) ->
        let (state, members) = map_list (see_decl v) state members in
        (state, RecordDecl (name, members))

    | FieldDecl (ty, name, bitwidth, init) ->
        let (state, ty) = see_type_loc v state ty in
        let (state, bitwidth) = map_opt (see_expr v) state bitwidth in
        let (state, init) = map_opt (see_expr v) state init in
        (state, FieldDecl (ty, name, bitwidth, init))

    | EnumDecl (name, enums) ->
        let (state, enums) = map_list (see_decl v) state enums in
        (state, EnumDecl (name, enums))

    | EnumConstantDecl (name, value) ->
        let (state, value) = map_opt (see_expr v) state value in
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
        (state, BinaryOperator (op, lhs, rhs))
    | UnaryOperator (op, operand) ->
        (state, UnaryOperator (op, operand))

    | DeclRefExpr name ->
        (state, DeclRefExpr name)
    | PredefinedExpr kind ->
        (state, PredefinedExpr kind)
    | ImplicitCastExpr (kind, expr) ->
        (state, ImplicitCastExpr (kind, expr))
    | CStyleCastExpr (kind, ty, expr) ->
        (state, CStyleCastExpr (kind, ty, expr))
    | CompoundLiteralExpr (ty, init) ->
        (state, CompoundLiteralExpr (ty, init))
    | ParenExpr expr ->
        (state, ParenExpr expr)
    | VAArgExpr (expr, ty) ->
        (state, VAArgExpr (expr, ty))
    | CallExpr (callee, args) ->
        (state, CallExpr (callee, args))
    | MemberExpr (base, member, is_arrow) ->
        (state, MemberExpr (base, member, is_arrow))
    | ConditionalOperator (cond, then_expr, else_expr) ->
        (state, ConditionalOperator (cond, then_expr, else_expr))
    | DesignatedInitExpr (desgs, init) ->
        (state, DesignatedInitExpr (desgs, init))
    | InitListExpr inits ->
        (state, InitListExpr inits)
    | ImplicitValueInitExpr ->
        (state, ImplicitValueInitExpr)
    | ArraySubscriptExpr (base, index) ->
        (state, ArraySubscriptExpr (base, index))
    | StmtExpr stmt ->
        (state, StmtExpr stmt)

    | SizeOfExpr expr ->
        (state, SizeOfExpr expr)
    | SizeOfType ty ->
        (state, SizeOfType ty)
    | AlignOfExpr expr ->
        (state, AlignOfExpr expr)
    | AlignOfType ty ->
        (state, AlignOfType ty)
    | VecStepExpr expr ->
        (state, VecStepExpr expr)
    | VecStepType ty ->
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
        (state, LabelStmt (label, stmt))
    | CaseStmt (range_start, range_end, stmt) ->
        (state, CaseStmt (range_start, range_end, stmt))
    | DefaultStmt stmt ->
        (state, DefaultStmt stmt)
    | GotoStmt label ->
        (state, GotoStmt label)
    | ExprStmt expr ->
        (state, ExprStmt expr)
    | CompoundStmt body ->
        (state, CompoundStmt body)
    | ReturnStmt expr ->
        (state, ReturnStmt expr)
    | IfStmt (cond, then_stmt, else_stmt) ->
        (state, IfStmt (cond, then_stmt, else_stmt))
    | ForStmt (init, cond, incr, body) ->
        (state, ForStmt (init, cond, incr, body))
    | WhileStmt (cond, body) ->
        (state, WhileStmt (cond, body))
    | DoStmt (body, cond) ->
        (state, DoStmt (body, cond))
    | SwitchStmt (expr, body) ->
        (state, SwitchStmt (expr, body))
    | DeclStmt decls ->
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
        (state, TypeOfExprType expr)
    | TypeOfType ty ->
        (state, TypeOfType ty)
    | ParenType ty ->
        (state, ParenType ty)
    | TypedefType name ->
        (state, TypedefType name)
    | PointerType pointee ->
        (state, PointerType pointee)
    | FunctionNoProtoType result ->
        (state, FunctionNoProtoType result)
    | FunctionProtoType (result, args) ->
        (state, FunctionProtoType (result, args))
    | ConstantArrayType (memty, size) ->
        (state, ConstantArrayType (memty, size))
    | VariableArrayType (memty, size) ->
        (state, VariableArrayType (memty, size))
    | IncompleteArrayType memty ->
        (state, IncompleteArrayType memty)
    | ElaboratedType ty ->
        (state, ElaboratedType ty)
    | EnumType name ->
        (state, EnumType name)
    | RecordType (kind, name) ->
        (state, RecordType (kind, name))
    | DecayedType (decayed, original) ->
        (state, DecayedType (decayed, original))
  in
  (state, { ctyp with t })


let visit_type_loc v state type_loc =
  let (state, tl) =
    match type_loc.tl with
    | UnimpTypeLoc name ->
        failwith name

    | BuiltinTypeLoc bt ->
        (state, BuiltinTypeLoc bt)
    | TypeOfExprTypeLoc expr ->
        (state, TypeOfExprTypeLoc expr)
    | TypeOfTypeLoc ty ->
        (state, TypeOfTypeLoc ty)
    | ParenTypeLoc ty ->
        (state, ParenTypeLoc ty)
    | QualifiedTypeLoc (ty, qual, aspace) ->
        (state, QualifiedTypeLoc (ty, qual, aspace))
    | TypedefTypeLoc name ->
        (state, TypedefTypeLoc name)
    | PointerTypeLoc pointee ->
        (state, PointerTypeLoc pointee)
    | FunctionNoProtoTypeLoc result ->
        (state, FunctionNoProtoTypeLoc result)
    | FunctionProtoTypeLoc (result, args) ->
        (state, FunctionProtoTypeLoc (result, args))
    | ConstantArrayTypeLoc (memty, size) ->
        (state, ConstantArrayTypeLoc (memty, size))
    | VariableArrayTypeLoc (memty, size) ->
        (state, VariableArrayTypeLoc (memty, size))
    | IncompleteArrayTypeLoc memty ->
        (state, IncompleteArrayTypeLoc memty)
    | ElaboratedTypeLoc ty ->
        (state, ElaboratedTypeLoc ty)
    | EnumTypeLoc name ->
        (state, EnumTypeLoc name)
    | RecordTypeLoc (kind, name) ->
        (state, RecordTypeLoc (kind, name))
  in
  (state, { type_loc with tl })
