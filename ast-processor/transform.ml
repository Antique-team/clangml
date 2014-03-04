open C_sig
open ClangAst
open Data_structures


let dump_stmt s =
  Format.fprintf Format.std_formatter "%a\n"
    ClangPp.pp_stmt s


let add_type name ty prog =
  { prog with cp_types = StringMap.add name ty prog.cp_types }

let add_fun name fn prog =
  { prog with cp_funs = StringMap.add name fn prog.cp_funs }

let add_var name var prog =
  { prog with cp_vars = StringMap.add name var prog.cp_vars }


let c_type_of_builtin_type = function
  | BT_Void -> Ctvoid
  | BT_Char16
  | BT_Char32
  | BT_Short
  | BT_Long
  | BT_LongLong
  | BT_Int128
  | BT_UShort
  | BT_ULong
  | BT_ULongLong
  | BT_UInt128
  | BT_UInt
  | BT_Int -> Ctint
  | BT_Bool
  | BT_SChar
  | BT_UChar
  | BT_Char_S
  | BT_Char_U -> Ctchar
  | _ -> Ctint


let rec c_type_of_type = function
  | BuiltinType bt ->
      c_type_of_builtin_type bt

  | ConstantArrayType (memty, size) ->
      Ctarray (c_type_of_type memty, size)

  | TypedefType (name) ->
      Ctnamed { cnt_name = name; cnt_type = Ctvoid; }

  | ElaboratedType (ty) ->
      c_type_of_type ty

  | EnumType (name)
  | RecordType (_, name) ->
      Ctnamed { cnt_name = name; cnt_type = Ctvoid; }

  | PointerType (pointee) ->
      Ctptr (Some (c_type_of_type pointee))

  | ParenType (inner) ->
      c_type_of_type inner

  | QualifiedType (unqual, quals, aspace) ->
      c_type_of_type unqual

  | TypeOfExprType _ -> failwith "TypeOfExprType"
  | TypeOfType _ -> failwith "TypeOfType"
  | FunctionProtoType _ -> failwith "FunctionProtoType"
  | FunctionNoProtoType _ -> failwith "FunctionNoProtoType"
  | VariableArrayType _ -> failwith "VariableArrayType"
  | IncompleteArrayType _ -> failwith "IncompleteArrayType"
  | DecayedType _ -> failwith "IncompleteArrayType"

  | UnimpType (name) -> failwith ("Unimplemented: " ^ name)


let rec c_type_of_type_loc = function
  | BuiltinTypeLoc (_, bt) ->
      c_type_of_builtin_type bt

  | ConstantArrayTypeLoc (_, memty, size) ->
      Ctarray (c_type_of_type_loc memty, size)

  | TypedefTypeLoc (_, name) ->
      Ctnamed { cnt_name = name; cnt_type = Ctvoid; }

  | ElaboratedTypeLoc (_, ty) ->
      c_type_of_type_loc ty

  | EnumTypeLoc (_, name)
  | RecordTypeLoc (_, _, name) ->
      Ctnamed { cnt_name = name; cnt_type = Ctvoid; }

  | PointerTypeLoc (_, pointee) ->
      Ctptr (Some (c_type_of_type_loc pointee))

  | ParenTypeLoc (_, inner) ->
      c_type_of_type_loc inner

  | FunctionProtoTypeLoc (sloc, _, _) as ty ->
      Format.fprintf Format.std_formatter "%a%a\n"
        ClangPp.pp_sloc sloc
        ClangPp.pp_tloc ty;
      failwith "FunctionProtoTypeLoc"

  | TypeOfExprTypeLoc _ -> failwith "TypeOfExprTypeLoc"
  | TypeOfTypeLoc _ -> failwith "TypeOfTypeLoc"
  | QualifiedTypeLoc _ -> failwith "QualifiedTypeLoc"
  | FunctionNoProtoTypeLoc _ -> failwith "FunctionNoProtoTypeLoc"
  | VariableArrayTypeLoc _ -> failwith "VariableArrayTypeLoc"
  | IncompleteArrayTypeLoc _ -> failwith "IncompleteArrayTypeLoc"

  | UnimpTypeLoc (_, name) -> failwith ("Unimplemented: " ^ name)


let c_agg_field_of_decl = function
  | FieldDecl (_, ty, name, bitwidth, init) ->
      if bitwidth <> None then failwith "Bit fields not implemented";
      if init <> None then failwith "Member initialisers not implemented";
      {
        caf_typ = c_type_of_type_loc ty;
        caf_off = 0;
        caf_size = 0;
        caf_name = name;
      }
  | _ -> failwith "Only FieldDecls allowed within RecordDecl"


let c_var_of_parm_decl = function
  | ParmVarDecl (_, ty, name) ->
      {
        cv_name     = name;
        cv_uid      = 0;
        cv_type     = c_type_of_type_loc ty;
        cv_volatile = false (* TODO: ClangQuery.is_volatile_tloc ty *);
      }
  | _ -> failwith "only ParmVarDecls allowed in function argument list"


let c_decl_of_decl = function
  | VarDecl ({ loc_s_line }, ty, name, init) ->
      if init <> None then
        failwith "Unsupported: initialiser in declaration";
      loc_s_line, {
        cv_name = name;
        cv_uid = 0;
        cv_type = c_type_of_type_loc ty;
        cv_volatile = false (* TODO: ClangQuery.is_volatile_tloc ty *);
      }

  | EmptyDecl _ ->
      failwith "empty declaration within function body"
  | FunctionDecl _ ->
      failwith "local function declarations are not supported by memcad AST"
  | TypedefDecl (_, ty, name) ->
      failwith "local typedefs are not supported by memcad AST"
  | EnumDecl (_, name, enumerators) ->
      failwith "local enums are not supported by memcad AST"
  | RecordDecl (_, name, members) ->
      failwith "local structs are not supported by memcad AST"

  | EnumConstantDecl    _ -> failwith "EnumConstantDecl found in function"
  | FieldDecl           _ -> failwith "FieldDecl found in function"
  | ParmVarDecl         _ -> failwith "ParmVarDecl found in function"
  | TranslationUnitDecl _ -> failwith "TranslationUnitDecl found in function"

  | UnimpDecl (_, name) -> failwith ("Unimplemented: " ^ name)


let rec c_lvalk_of_expr = function
  | TypedExpr (expr, ty) ->
      (* Ignore type within sub-expressions. *)
      c_lvalk_of_expr expr

  | IntegerLiteral _ -> failwith "IntegerLiteral"
  | CharacterLiteral _ -> failwith "CharacterLiteral"
  | FloatingLiteral _ -> failwith "FloatingLiteral"
  | StringLiteral _ -> failwith "StringLiteral"
  | BinaryOperator _ -> failwith "BinaryOperator"
  | UnaryOperator _ -> failwith "UnaryOperator"

  | DeclRefExpr (_, name) -> failwith "DeclRefExpr"
  | PredefinedExpr _ -> failwith "PredefinedExpr"
  | ImplicitCastExpr _ -> failwith "ImplicitCastExpr"
  | CStyleCastExpr _ -> failwith "CStyleCastExpr"
  | CompoundLiteralExpr _ -> failwith "CompoundLiteralExpr"
  | ParenExpr _ -> failwith "ParenExpr"
  | VAArgExpr _ -> failwith "VAArgExpr"
  | CallExpr _ -> failwith "CallExpr"
  | MemberExpr _ -> failwith "MemberExpr"
  | ConditionalOperator _ -> failwith "ConditionalOperator"
  | DesignatedInitExpr _ -> failwith "DesignatedInitExpr"
  | InitListExpr _ -> failwith "InitListExpr"
  | ImplicitValueInitExpr _ -> failwith "ImplicitValueInitExpr"
  | ArraySubscriptExpr _ -> failwith "ArraySubscriptExpr"
  | StmtExpr _ -> failwith "StmtExpr"

  | SizeOfExpr _ -> failwith "SizeOfExpr"
  | SizeOfType _ -> failwith "SizeOfType"
  | AlignOfExpr _ -> failwith "AlignOfExpr"
  | AlignOfType _ -> failwith "AlignOfType"
  | VecStepExpr _ -> failwith "VecStepExpr"
  | VecStepType _ -> failwith "VecStepType"

  | UnimpExpr (_, name) -> failwith ("Unimplemented expr: " ^ name)
let c_lval_of_expr = function
  | TypedExpr (expr, ty) ->
      {
        clk = c_lvalk_of_expr expr;
        clt = c_type_of_type ty;
      }
  | _ -> failwith "typed expression required in lvalue"


let rec c_exprk_of_expr = function
  | TypedExpr (expr, ty) ->
      (* Ignore type within sub-expressions. *)
      c_exprk_of_expr expr

  | IntegerLiteral (_, i) ->
      Ceconst (Ccint i)

  | CharacterLiteral _ -> failwith "CharacterLiteral"
  | FloatingLiteral _ -> failwith "FloatingLiteral"
  | StringLiteral _ -> failwith "StringLiteral"
  | BinaryOperator _ -> failwith "BinaryOperator"
  | UnaryOperator _ -> failwith "UnaryOperator"

  | DeclRefExpr _ -> failwith "DeclRefExpr"
  | PredefinedExpr _ -> failwith "PredefinedExpr"
  | ImplicitCastExpr _ -> failwith "ImplicitCastExpr"
  | CStyleCastExpr _ -> failwith "CStyleCastExpr"
  | CompoundLiteralExpr _ -> failwith "CompoundLiteralExpr"
  | ParenExpr _ -> failwith "ParenExpr"
  | VAArgExpr _ -> failwith "VAArgExpr"
  | CallExpr _ -> failwith "CallExpr"
  | MemberExpr _ -> failwith "MemberExpr"
  | ConditionalOperator _ -> failwith "ConditionalOperator"
  | DesignatedInitExpr _ -> failwith "DesignatedInitExpr"
  | InitListExpr _ -> failwith "InitListExpr"
  | ImplicitValueInitExpr _ -> failwith "ImplicitValueInitExpr"
  | ArraySubscriptExpr _ -> failwith "ArraySubscriptExpr"
  | StmtExpr _ -> failwith "StmtExpr"

  | SizeOfExpr _ -> failwith "SizeOfExpr"
  | SizeOfType _ -> failwith "SizeOfType"
  | AlignOfExpr _ -> failwith "AlignOfExpr"
  | AlignOfType _ -> failwith "AlignOfType"
  | VecStepExpr _ -> failwith "VecStepExpr"
  | VecStepType _ -> failwith "VecStepType"

  | UnimpExpr (_, name) -> failwith ("Unimplemented expr: " ^ name)


let c_expr_of_expr = function
  | TypedExpr (expr, ty) ->
      {
        cek = c_exprk_of_expr expr;
        cet = c_type_of_type ty;
      }
  | _ -> failwith "typed expression required in rvalue"


let rec c_stats_of_expr stats = function
  | BinaryOperator ({ loc_s_line }, BO_Assign, lhs, rhs) ->
      {
        csl = loc_s_line;
        csk = Csassign (c_lval_of_expr lhs, c_expr_of_expr rhs);
      }
      :: stats

  | TypedExpr (expr, ty) ->
      c_stats_of_expr stats expr

  | IntegerLiteral _ -> failwith "IntegerLiteral"
  | CharacterLiteral _ -> failwith "CharacterLiteral"
  | FloatingLiteral _ -> failwith "FloatingLiteral"
  | StringLiteral _ -> failwith "StringLiteral"
  | BinaryOperator _ -> failwith "BinaryOperator"
  | UnaryOperator _ -> failwith "UnaryOperator"

  | DeclRefExpr _ -> failwith "DeclRefExpr"
  | PredefinedExpr _ -> failwith "PredefinedExpr"
  | ImplicitCastExpr _ -> failwith "ImplicitCastExpr"
  | CStyleCastExpr _ -> failwith "CStyleCastExpr"
  | CompoundLiteralExpr _ -> failwith "CompoundLiteralExpr"
  | ParenExpr _ -> failwith "ParenExpr"
  | VAArgExpr _ -> failwith "VAArgExpr"
  | CallExpr _ -> failwith "CallExpr"
  | MemberExpr _ -> failwith "MemberExpr"
  | ConditionalOperator _ -> failwith "ConditionalOperator"
  | DesignatedInitExpr _ -> failwith "DesignatedInitExpr"
  | InitListExpr _ -> failwith "InitListExpr"
  | ImplicitValueInitExpr _ -> failwith "ImplicitValueInitExpr"
  | ArraySubscriptExpr _ -> failwith "ArraySubscriptExpr"
  | StmtExpr _ -> failwith "StmtExpr"

  | SizeOfExpr _ -> failwith "SizeOfExpr"
  | SizeOfType _ -> failwith "SizeOfType"
  | AlignOfExpr _ -> failwith "AlignOfExpr"
  | AlignOfType _ -> failwith "AlignOfType"
  | VecStepExpr _ -> failwith "VecStepExpr"
  | VecStepType _ -> failwith "VecStepType"

  | UnimpExpr (_, name) -> failwith ("Unimplemented expr: " ^ name)


(* This function maps N clang statements to M memcad statements.
   M may be considerably more than N. *)
let rec c_stats_of_stmts stats = function
  | [] -> stats

  | ExprStmt e :: tl ->
      c_stats_of_stmts (c_stats_of_expr stats e) tl

  | NullStmt _ :: tl -> failwith "NullStmt"
  | BreakStmt _ :: tl -> failwith "BreakStmt"
  | ContinueStmt _ :: tl -> failwith "ContinueStmt"
  | LabelStmt _ :: tl -> failwith "LabelStmt"
  | CaseStmt _ :: tl -> failwith "CaseStmt"
  | DefaultStmt _ :: tl -> failwith "DefaultStmt"
  | GotoStmt _ :: tl -> failwith "GotoStmt"
  | CompoundStmt _ :: tl -> failwith "CompoundStmt"
  | ReturnStmt _ :: tl -> failwith "ReturnStmt"
  | IfStmt _ :: tl -> failwith "IfStmt"
  | ForStmt _ :: tl -> failwith "ForStmt"
  | WhileStmt _ :: tl -> failwith "WhileStmt"
  | DoStmt _ :: tl -> failwith "DoStmt"
  | SwitchStmt _ :: tl -> failwith "SwitchStmt"
  | DeclStmt (_, decls) :: tl ->
      let stats =
        List.map (fun (loc, d) -> { csl = loc; csk = Csdecl d })
          (List.map c_decl_of_decl decls)
        @ stats
      in
      c_stats_of_stmts stats tl

  | UnimpStmt (_, name) :: tl -> failwith ("Unimplemented statement: " ^ name)


let c_fun_of_decl ty name body =
  let stmts = ClangQuery.body_of_compound_stmt body in
  {
    cf_type = c_type_of_type_loc (ClangQuery.return_type_of_tloc ty);
    cf_uid  = 0;
    cf_name = name;
    cf_args = List.map c_var_of_parm_decl (ClangQuery.args_of_tloc ty);
    cf_body = c_stats_of_stmts [] stmts;
  }


let rec collect_decls prog = function
  | [] -> prog

  | EmptyDecl _ :: tl ->
      collect_decls prog tl

  | FunctionDecl { fd_body = None; } :: tl ->
      (* Function declarations (without definition) do nothing. *)
      collect_decls prog tl

  | FunctionDecl { fd_type; fd_name; fd_body = Some body; } :: tl ->
      let c_fun = c_fun_of_decl fd_type fd_name body in
      collect_decls (add_fun fd_name c_fun prog) tl

  (*
    Clang turns this code:
      typedef struct foo { int a; } bar;
    into this:
      struct foo { int a; };
      typedef struct foo bar;
    but there is no way to express these things in the memcad AST,
    and the memcad parser doesn't parse it, so we match this construct
    explicitly and transform it to the appropriate memcad AST.
   *)
  | RecordDecl (_, name1, members)
    :: TypedefDecl (_, ElaboratedTypeLoc
                      (_, RecordTypeLoc (_, kind,
                                         name2)), name)
    :: tl
    when name1 = name2 ->
      let c_type =
        let agg = {
          cag_name = if name1 = "" then None else Some name1;
          cag_align = 0;
          cag_size = 0;
          cag_fields = List.map c_agg_field_of_decl members;
        } in
        match kind with
        | TTK_Struct -> Ctstruct agg
        | TTK_Union -> Ctunion agg
        | _ -> failwith "Unhandled tag type kind"
      in
      collect_decls (add_type name c_type prog) tl

  | TypedefDecl (_, ty, name) :: tl ->
      let c_type = c_type_of_type_loc ty in
      collect_decls (add_type name c_type prog) tl

  | VarDecl (_, ty, name, init) :: tl ->
      collect_decls prog tl
  | EnumDecl (_, name, enumerators) :: tl ->
      collect_decls prog tl
  | RecordDecl (_, name, members) :: tl ->
      failwith "RecordDecl without TypedefDecl not supported by memcad AST"

  | EnumConstantDecl    _ :: _ -> failwith "EnumConstantDecl found at file scope"
  | FieldDecl           _ :: _ -> failwith "FieldDecl found at file scope"
  | ParmVarDecl         _ :: _ -> failwith "ParmVarDecl found at file scope"
  | TranslationUnitDecl _ :: _ -> failwith "nested TranslationUnitDecl found"

  | UnimpDecl (_, name) :: _ -> failwith ("Unimplemented: " ^ name)


let c_prog_from_decl = function
  | TranslationUnitDecl (_, decls) ->
      collect_decls C_utils.empty_unit decls
  | _ -> failwith "c_prog_from_decl requires a translation unit"
