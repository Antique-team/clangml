open C_sig
open ClangAst
open Data_structures


let empty_env = StringMap.empty


let lookup_var prog env name =
  try
    StringMap.find name env
  with Not_found ->
    StringMap.find name prog.cp_vars


let dump_stmt s =
  Format.fprintf Format.std_formatter "%a\n"
    ClangPp.pp_stmt s

let dump_expr s =
  Format.fprintf Format.std_formatter "%a\n"
    ClangPp.pp_expr s


let add_type name ty prog =
  { prog with cp_types = StringMap.add name ty prog.cp_types }

let add_fun name fn prog =
  { prog with cp_funs = StringMap.add name fn prog.cp_funs }

let add_var name var prog =
  { prog with cp_vars = StringMap.add name var prog.cp_vars }


let c_binop_of_binop = function
  | BO_EQ -> Cbeq
  | BO_NE -> Cbne
  | BO_GE -> Cbge
  | BO_GT -> Cbgt
  | BO_LE -> Cble
  | BO_LT -> Cblt
  | BO_Add -> Cbadd
  | BO_Sub -> Cbsub
  | BO_Mul -> Cbmul
  | BO_LAnd -> Cbland
  | BO_LOr -> Cblor
  | _ -> failwith "unsupported binop"


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
  | { tl = BuiltinTypeLoc bt } ->
      c_type_of_builtin_type bt

  | { tl = ConstantArrayTypeLoc (memty, size) } ->
      Ctarray (c_type_of_type_loc memty, size)

  | { tl = TypedefTypeLoc name } ->
      Ctnamed { cnt_name = name; cnt_type = Ctvoid; }

  | { tl = ElaboratedTypeLoc ty } ->
      c_type_of_type_loc ty

  | { tl = EnumTypeLoc name }
  | { tl = RecordTypeLoc (_, name) } ->
      Ctnamed { cnt_name = name; cnt_type = Ctvoid; }

  | { tl = PointerTypeLoc (pointee) } ->
      Ctptr (Some (c_type_of_type_loc pointee))

  | { tl = ParenTypeLoc (inner) } ->
      c_type_of_type_loc inner

  | { tl = FunctionProtoTypeLoc (_, _); tl_sloc = sloc; } as ty ->
      Format.fprintf Format.std_formatter "%a%a\n"
        ClangPp.pp_sloc sloc
        ClangPp.pp_tloc ty;
      failwith "FunctionProtoTypeLoc"

  | { tl = TypeOfExprTypeLoc _ } -> failwith "TypeOfExprTypeLoc"
  | { tl = TypeOfTypeLoc _ } -> failwith "TypeOfTypeLoc"
  | { tl = QualifiedTypeLoc _ } -> failwith "QualifiedTypeLoc"
  | { tl = FunctionNoProtoTypeLoc _ } -> failwith "FunctionNoProtoTypeLoc"
  | { tl = VariableArrayTypeLoc _ } -> failwith "VariableArrayTypeLoc"
  | { tl = IncompleteArrayTypeLoc _ } -> failwith "IncompleteArrayTypeLoc"

   | { tl = UnimpTypeLoc name } -> failwith ("Unimplemented: " ^ name)


let c_agg_field_of_decl = function
  | { d = FieldDecl (ty, name, bitwidth, init) } ->
      if bitwidth <> None then failwith "Bit fields not implemented";
      if init <> None then failwith "Member initialisers not implemented";
      {
        caf_typ = c_type_of_type_loc ty;
        caf_off = -1;
        caf_size = -1;
        caf_name = name;
      }
  | _ -> failwith "Only FieldDecls allowed within RecordDecl"


let c_var_of_parm_decl = function
  | { d = ParmVarDecl (ty, name) } ->
      {
        cv_name     = name;
        cv_uid      = -1;
        cv_type     = c_type_of_type_loc ty;
        cv_volatile = false (* TODO: ClangQuery.is_volatile_tloc ty *);
      }
  | _ -> failwith "only ParmVarDecls allowed in function argument list"


let c_decl_of_decl { d_sloc = { loc_s_line }; d } =
  match d with
  | VarDecl (ty, name, init) ->
      if init <> None then
        failwith "Unsupported: initialiser in declaration";
      loc_s_line, {
        cv_name = name;
        cv_uid = -1;
        cv_type = c_type_of_type_loc ty;
        cv_volatile = false (* TODO: ClangQuery.is_volatile_tloc ty *);
      }

  | EmptyDecl ->
      failwith "empty declaration within function body"
  | FunctionDecl _ ->
      failwith "local function declarations are not supported by memcad AST"
  | TypedefDecl (ty, name) ->
      failwith "local typedefs are not supported by memcad AST"
  | EnumDecl (name, enumerators) ->
      failwith "local enums are not supported by memcad AST"
  | RecordDecl (name, members) ->
      failwith "local structs are not supported by memcad AST"

  | EnumConstantDecl    _ -> failwith "EnumConstantDecl found in function"
  | FieldDecl           _ -> failwith "FieldDecl found in function"
  | ParmVarDecl         _ -> failwith "ParmVarDecl found in function"
  | TranslationUnitDecl _ -> failwith "TranslationUnitDecl found in function"

  | UnimpDecl name -> failwith ("Unimplemented: " ^ name)


let rec c_lvalk_of_expr prog env = function
  | TypedExpr (expr, ty) ->
      (* Ignore type within sub-expressions. *)
      c_lvalk_of_expr prog env expr

  | DeclRefExpr (_, name) ->
      Clvar (lookup_var prog env name)

  | MemberExpr (_, (TypedExpr (_, ty) as base), member, true) ->
      (*print_endline (Show.show<ClangShow.ctyp> ty);*)
      let deref =
        {
          clk = Clderef (c_expr_of_expr prog env base);
          clt = c_type_of_type ty;
        }
      in
      Clfield (deref, member)

  | MemberExpr (_, _, _, false) ->
      failwith "field access of (non-pointer) object in lval"

  | MemberExpr (_, _, _, _) ->
      failwith "invalid base of MemberExpr"

  | IntegerLiteral _ -> failwith "lvalk IntegerLiteral"
  | CharacterLiteral _ -> failwith "lvalk CharacterLiteral"
  | FloatingLiteral _ -> failwith "lvalk FloatingLiteral"
  | StringLiteral _ -> failwith "lvalk StringLiteral"
  | BinaryOperator _ -> failwith "lvalk BinaryOperator"
  | UnaryOperator _ -> failwith "lvalk UnaryOperator"

  | PredefinedExpr _ -> failwith "lvalk PredefinedExpr"
  | ImplicitCastExpr _ -> failwith "lvalk ImplicitCastExpr"
  | CStyleCastExpr _ -> failwith "lvalk CStyleCastExpr"
  | CompoundLiteralExpr _ -> failwith "lvalk CompoundLiteralExpr"
  | ParenExpr _ -> failwith "lvalk ParenExpr"
  | VAArgExpr _ -> failwith "lvalk VAArgExpr"
  | CallExpr _ -> failwith "lvalk CallExpr"
  | ConditionalOperator _ -> failwith "lvalk ConditionalOperator"
  | DesignatedInitExpr _ -> failwith "lvalk DesignatedInitExpr"
  | InitListExpr _ -> failwith "lvalk InitListExpr"
  | ImplicitValueInitExpr _ -> failwith "lvalk ImplicitValueInitExpr"
  | ArraySubscriptExpr _ -> failwith "lvalk ArraySubscriptExpr"
  | StmtExpr _ -> failwith "lvalk StmtExpr"

  | SizeOfExpr _ -> failwith "lvalk SizeOfExpr"
  | SizeOfType _ -> failwith "lvalk SizeOfType"
  | AlignOfExpr _ -> failwith "lvalk AlignOfExpr"
  | AlignOfType _ -> failwith "lvalk AlignOfType"
  | VecStepExpr _ -> failwith "lvalk VecStepExpr"
  | VecStepType _ -> failwith "lvalk VecStepType"

  | UnimpExpr (_, name) -> failwith ("Unimplemented expr: " ^ name)


and c_lval_of_expr prog env = function
  | TypedExpr (expr, ty) ->
      {
        clk = c_lvalk_of_expr prog env expr;
        clt = c_type_of_type ty;
      }
  | _ -> failwith "typed expression required in lvalue"


and c_exprk_of_expr prog env = function
  | ImplicitCastExpr (_, expr)
  | TypedExpr (expr, _) ->
      (* Ignore type within sub-expressions. *)
      c_exprk_of_expr prog env expr

  | IntegerLiteral (_, i) ->
      Ceconst (Ccint i)

  | BinaryOperator (_, op, lhs, rhs) ->
      Cebin (
        c_binop_of_binop op,
        c_expr_of_expr prog env lhs,
        c_expr_of_expr prog env rhs
      )

  | ParenExpr (_, expr) ->
      c_exprk_of_expr prog env expr

  | CStyleCastExpr _ -> failwith "exprk CStyleCastExpr"

  | CharacterLiteral _ -> failwith "exprk CharacterLiteral"
  | FloatingLiteral _ -> failwith "exprk FloatingLiteral"
  | StringLiteral _ -> failwith "exprk StringLiteral"
  | UnaryOperator _ -> failwith "exprk UnaryOperator"

  | PredefinedExpr _ -> failwith "exprk PredefinedExpr"
  | CompoundLiteralExpr _ -> failwith "exprk CompoundLiteralExpr"
  | VAArgExpr _ -> failwith "exprk VAArgExpr"
  | CallExpr _ -> failwith "exprk CallExpr"
  | MemberExpr _ -> failwith "exprk MemberExpr"
  | ConditionalOperator _ -> failwith "exprk ConditionalOperator"
  | DesignatedInitExpr _ -> failwith "exprk DesignatedInitExpr"
  | InitListExpr _ -> failwith "exprk InitListExpr"
  | ImplicitValueInitExpr _ -> failwith "exprk ImplicitValueInitExpr"
  | ArraySubscriptExpr _ -> failwith "exprk ArraySubscriptExpr"
  | StmtExpr _ -> failwith "exprk StmtExpr"
  | DeclRefExpr _ -> failwith "exprk DeclRefExpr"

  | SizeOfExpr _ -> failwith "exprk SizeOfExpr"
  | SizeOfType _ -> failwith "exprk SizeOfType"
  | AlignOfExpr _ -> failwith "exprk AlignOfExpr"
  | AlignOfType _ -> failwith "exprk AlignOfType"
  | VecStepExpr _ -> failwith "exprk VecStepExpr"
  | VecStepType _ -> failwith "exprk VecStepType"

  | UnimpExpr (_, name) -> failwith ("Unimplemented expr: " ^ name)


and c_expr_of_expr prog env = function
  (* (( void * )0) => null *)
  | TypedExpr (
      CStyleCastExpr (_,
        { tl = PointerTypeLoc
          { tl = BuiltinTypeLoc BT_Void }
        },
        TypedExpr (IntegerLiteral (_, 0), _)
      ),
    _) ->
      {
        cek = Ceconst Ccnull;
        cet = Ctptr None;
      }

  | TypedExpr (MemberExpr _, ty)
  | TypedExpr (DeclRefExpr _, ty) as expr ->
      (*print_endline (Show.show<ClangShow.expr> expr);*)
      (*print_endline (Show.show<ClangShow.ctyp> ty);*)
      {
        cek = Celval (c_lval_of_expr prog env expr);
        cet = c_type_of_type ty;
      }

  | TypedExpr (expr, ty) ->
      (*print_endline (Show.show<ClangShow.expr> expr);*)
      {
        cek = c_exprk_of_expr prog env expr;
        cet = c_type_of_type ty;
      }

  | _ -> failwith "typed expression required in rvalue"


let rec c_stat_of_expr prog env = function
  | BinaryOperator ({ loc_s_line }, BO_Assign, lhs, rhs) ->
      (*print_endline (Show.show<ClangShow.expr> rhs);*)
      {
        csl = loc_s_line;
        csk = Csassign (
          c_lval_of_expr prog env lhs,
          c_expr_of_expr prog env rhs
        );
      }

  | CallExpr ({ loc_s_line = csl }, callee, args) ->
      let csk =
        match ClangQuery.identifier_of_expr callee,
              List.map ClangUntype.strip_expr args with
        | "_memcad", [StringLiteral (_, str)] ->
            Cs_memcad (Mc_comstring str)
        | "assert", [_] ->
            Csassert (List.hd args |> c_expr_of_expr prog env)
        | "free", [_] ->
            Csfree (List.hd args |> c_lval_of_expr prog env)
        | _ ->
            Cspcall {
              cc_fun = c_expr_of_expr prog env callee;
              cc_args = List.map (c_expr_of_expr prog env) args;
            }
      in
      { csl; csk; }

  | TypedExpr (expr, ty) ->
      c_stat_of_expr prog env expr

  | IntegerLiteral _ -> failwith "stats IntegerLiteral"
  | CharacterLiteral _ -> failwith "stats CharacterLiteral"
  | FloatingLiteral _ -> failwith "stats FloatingLiteral"
  | StringLiteral _ -> failwith "stats StringLiteral"
  | BinaryOperator _ -> failwith "stats BinaryOperator"
  | UnaryOperator _ -> failwith "stats UnaryOperator"

  | DeclRefExpr _ -> failwith "stats DeclRefExpr"
  | PredefinedExpr _ -> failwith "stats PredefinedExpr"
  | ImplicitCastExpr _ -> failwith "stats ImplicitCastExpr"
  | CStyleCastExpr _ -> failwith "stats CStyleCastExpr"
  | CompoundLiteralExpr _ -> failwith "stats CompoundLiteralExpr"
  | ParenExpr _ -> failwith "stats ParenExpr"
  | VAArgExpr _ -> failwith "stats VAArgExpr"
  | MemberExpr _ -> failwith "stats MemberExpr"
  | ConditionalOperator _ -> failwith "stats ConditionalOperator"
  | DesignatedInitExpr _ -> failwith "stats DesignatedInitExpr"
  | InitListExpr _ -> failwith "stats InitListExpr"
  | ImplicitValueInitExpr _ -> failwith "stats ImplicitValueInitExpr"
  | ArraySubscriptExpr _ -> failwith "stats ArraySubscriptExpr"
  | StmtExpr _ -> failwith "stats StmtExpr"

  | SizeOfExpr _ -> failwith "stats SizeOfExpr"
  | SizeOfType _ -> failwith "stats SizeOfType"
  | AlignOfExpr _ -> failwith "stats AlignOfExpr"
  | AlignOfType _ -> failwith "stats AlignOfType"
  | VecStepExpr _ -> failwith "stats VecStepExpr"
  | VecStepType _ -> failwith "stats VecStepType"

  | UnimpExpr (_, name) -> failwith ("Unimplemented expr: " ^ name)


(* This function maps N clang statements to M memcad statements.
   M may be considerably more than N. *)
let rec c_stats_of_stmts prog env stmts =
  let rec loop env stats = function
    | [] -> stats

    | ExprStmt e :: tl ->
        let e = ClangImplicitCast.strip_expr e in
        loop env (c_stat_of_expr prog env e :: stats) tl

    | DeclStmt (_, [decl]) :: tl ->
        let (loc, c_decl) = c_decl_of_decl decl in
        let env = StringMap.add c_decl.cv_name c_decl env in
        let stats =
          { csl = loc; csk = Csdecl c_decl }
          :: stats
        in
        loop env stats tl

    | WhileStmt ({ loc_s_line = csl }, cond, body) :: tl ->
        let cond = ClangImplicitCast.strip_expr cond in
        let stats =
          let stmts = ClangQuery.body_of_stmt body in
          {
            csl;
            csk = Cswhile (
              c_expr_of_expr prog env cond,
              c_stats_of_stmts prog env stmts,
              None
            );
          }
          :: stats
        in
        loop env stats tl

    | IfStmt ({ loc_s_line = csl }, cond, then_body, else_body) :: tl ->
        let cond = ClangImplicitCast.strip_expr cond in
        let stats =
          let then_stmts = ClangQuery.body_of_stmt then_body in
          let else_stmts =
            match else_body with
            | None -> []
            | Some else_body -> ClangQuery.body_of_stmt else_body
          in
          {
            csl;
            csk = Csif (
              c_expr_of_expr prog env cond,
              c_stats_of_stmts prog env then_stmts,
              c_stats_of_stmts prog env else_stmts
            );
          }
          :: stats
        in
        loop env stats tl

    | NullStmt _ :: tl -> failwith "NullStmt"
    | BreakStmt _ :: tl -> failwith "BreakStmt"
    | ContinueStmt _ :: tl -> failwith "ContinueStmt"
    | LabelStmt _ :: tl -> failwith "LabelStmt"
    | CaseStmt _ :: tl -> failwith "CaseStmt"
    | DefaultStmt _ :: tl -> failwith "DefaultStmt"
    | GotoStmt _ :: tl -> failwith "GotoStmt"
    | CompoundStmt _ :: tl -> failwith "CompoundStmt"
    | ReturnStmt _ :: tl -> failwith "ReturnStmt"
    | ForStmt _ :: tl -> failwith "ForStmt"
    | DoStmt _ :: tl -> failwith "DoStmt"
    | SwitchStmt _ :: tl -> failwith "SwitchStmt"
    | DeclStmt _ :: tl -> failwith "DeclStmt"

    | UnimpStmt (_, name) :: tl -> failwith ("Unimplemented statement: " ^ name)
  in
  (* We build the list in reverse. *)
  List.rev (loop env [] stmts)


let c_fun_of_decl prog env ty name body =
  let stmts = ClangQuery.body_of_stmt body in
  {
    cf_type = c_type_of_type_loc (ClangQuery.return_type_of_tloc ty.tl);
    cf_uid  = -1;
    cf_name = name;
    cf_args = List.map c_var_of_parm_decl (ClangQuery.args_of_tloc ty.tl);
    cf_body = c_stats_of_stmts prog env stmts;
  }


let rec collect_decls prog env = function
  | [] -> prog

  | { d = EmptyDecl } :: tl ->
      collect_decls prog env tl

  | { d = FunctionDecl (_, _, None) } :: tl ->
      (* Function declarations (without definition) do nothing. *)
      collect_decls prog env tl

  | { d = FunctionDecl (fd_type, fd_name, Some body) } :: tl ->
      let c_fun = c_fun_of_decl prog env fd_type fd_name body in
      collect_decls (add_fun fd_name c_fun prog) env tl

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
  |   { d = RecordDecl (name1, members) }
    :: { d = TypedefDecl ({ tl = ElaboratedTypeLoc {
        tl = RecordTypeLoc (kind, name2)
      } }, name) }
    :: tl
    when name1 = name2 ->
      let c_type =
        let agg = {
          cag_name = if name1 = "" then None else Some name1;
          cag_align = -1;
          cag_size = -1;
          cag_fields = List.map c_agg_field_of_decl members;
        } in
        match kind with
        | TTK_Struct -> Ctstruct agg
        | TTK_Union -> Ctunion agg
        | _ -> failwith "Unhandled tag type kind"
      in
      collect_decls (add_type name c_type prog) env tl

  | { d = TypedefDecl (ty, name) } :: tl ->
      let c_type = c_type_of_type_loc ty in
      collect_decls (add_type name c_type prog) env tl

  | { d = VarDecl (ty, name, init) } :: tl ->
      collect_decls prog env tl
  | { d = EnumDecl (name, enumerators) } :: tl ->
      collect_decls prog env tl
  | { d = RecordDecl (name, members) } :: tl ->
      failwith "RecordDecl without TypedefDecl not supported by memcad AST"

  | { d = EnumConstantDecl    _ } :: _ -> failwith "EnumConstantDecl found at file scope"
  | { d = FieldDecl           _ } :: _ -> failwith "FieldDecl found at file scope"
  | { d = ParmVarDecl         _ } :: _ -> failwith "ParmVarDecl found at file scope"
  | { d = TranslationUnitDecl _ } :: _ -> failwith "nested TranslationUnitDecl found"

  | { d = UnimpDecl name } :: _ -> failwith ("Unimplemented: " ^ name)


let c_prog_from_decl = function
  | { d = TranslationUnitDecl decls } ->
      collect_decls C_utils.empty_unit empty_env decls
  | _ -> failwith "c_prog_from_decl requires a translation unit"
