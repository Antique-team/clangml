open C_sig
open Clang.Ast
open Data_structures

module Log = Logger.Make(struct let tag = "transform" end)


let lookup_var prog env name =
  try
    StringMap.find name env
  with Not_found ->
    try
      StringMap.find name prog.cp_vars
    with Not_found ->
      try
        let func = StringMap.find name prog.cp_funs in
        (* TODO: funcs are not vars, so how are they handled in DeclRefExprs? *)
        {
          cv_name     = func.cf_name;
          cv_uid      = func.cf_uid;
          cv_type     = func.cf_type;
          cv_volatile = false;
        }
      with Not_found ->
        Log.err "lookup failed for var: %s" name


let dump_stmt s =
  Format.printf "%a\n"
    Clang.Pp.pp_stmt s

let dump_expr s =
  Format.printf "%a\n"
    Clang.Pp.pp_expr s


let add_type name ty prog =
  assert (not (StringMap.mem name prog.cp_types));
  { prog with cp_types = StringMap.add name ty prog.cp_types }

let add_fun name fn prog =
  assert (not (StringMap.mem name prog.cp_funs));
  { prog with cp_funs = StringMap.add name fn prog.cp_funs }

let add_var name var prog =
  assert (not (StringMap.mem name prog.cp_vars));
  { prog with cp_vars = StringMap.add name var prog.cp_vars }


let c_binop_of_binary_operator = function
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
  | _ -> Log.unimp "unsupported binop"


let c_uniop_of_unary_operator = function
  | UO_Minus -> Cuneg
  | op -> Log.unimp "unsupported uniop: %a"
            Show.format<unary_operator> op


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
      Ctarray (c_type_of_type memty.t, size)

  | TypedefType name ->
      Ctnamed { cnt_name = name; cnt_type = Ctvoid; }

  | ElaboratedType ty ->
      c_type_of_type ty.t

  | EnumType name
  | RecordType (_, name) ->
      (* TODO: this is wrong. *)
      Ctnamed { cnt_name = name; cnt_type = Ctvoid; }

  | PointerType pointee ->
      Ctptr (Some (c_type_of_type pointee.t))

  | ParenType inner ->
      c_type_of_type inner.t

  | FunctionNoProtoType _
  | FunctionProtoType _ ->
      (* TODO *)
      Ctnamed { cnt_name = "FunctionProtoType"; cnt_type = Ctvoid; }

  | TypeOfExprType _ -> Log.unimp "TypeOfExprType"
  | TypeOfType _ -> Log.unimp "TypeOfType"
  | VariableArrayType _ -> Log.unimp "VariableArrayType"
  | IncompleteArrayType _ -> Log.unimp "IncompleteArrayType"
  | DecayedType _ -> Log.unimp "IncompleteArrayType"

  | UnimpType name -> Log.unimp "%s" name


let rec c_type_of_type_loc tl =
  match tl.tl with
  | BuiltinTypeLoc bt ->
      c_type_of_builtin_type bt

  | ConstantArrayTypeLoc (memty, size) ->
      Ctarray (c_type_of_type_loc memty, size)

  | TypedefTypeLoc name ->
      Ctnamed { cnt_name = name; cnt_type = Ctvoid; }

  | ElaboratedTypeLoc ty ->
      c_type_of_type_loc ty

  | EnumTypeLoc name
  | RecordTypeLoc (_, name) ->
      Ctnamed { cnt_name = name; cnt_type = Ctvoid; }

  | PointerTypeLoc pointee ->
      Ctptr (Some (c_type_of_type_loc pointee))

  | ParenTypeLoc inner ->
      c_type_of_type_loc inner

  | QualifiedTypeLoc (unqual, qual, aspace) ->
      c_type_of_type_loc unqual

  | FunctionProtoTypeLoc (_, _) ->
      Format.printf "%a%a\n"
        Clang.Pp.pp_sloc tl.tl_sloc
        Clang.Pp.pp_tloc tl;
      Log.unimp "FunctionProtoTypeLoc"

  | TypeOfExprTypeLoc _ -> Log.unimp "TypeOfExprTypeLoc"
  | TypeOfTypeLoc _ -> Log.unimp "TypeOfTypeLoc"
  | FunctionNoProtoTypeLoc _ -> Log.unimp "FunctionNoProtoTypeLoc"
  | VariableArrayTypeLoc _ -> Log.unimp "VariableArrayTypeLoc"
  | IncompleteArrayTypeLoc _ -> Log.unimp "IncompleteArrayTypeLoc"

  | UnimpTypeLoc name -> Log.unimp "%s" name


let make_aggregate agg = function
  | TTK_Struct -> Ctstruct agg
  | TTK_Union  -> Ctunion  agg
  | _ -> Log.unimp "Unhandled tag type kind"


let rec c_agg_fields_of_decls decls =
  let rec loop fields = function
    | [] -> fields

    | { d = RecordDecl (name1, members) }
      :: { d = FieldDecl ({ tl = ElaboratedTypeLoc {
          tl = RecordTypeLoc (kind, name2)
        } }, name, bitwidth, init) } :: tl
      when name1 = name2 ->
        if bitwidth <> None then
          Log.unimp "Bit fields not implemented";
        if init <> None then
          Log.unimp "Member initialisers not implemented";

        let agg = {
          cag_name   = if name1 = "" then None else Some name1;
          cag_align  = -1;
          cag_size   = -1;
          cag_fields = c_agg_fields_of_decls members;
        } in

        let field = {
          caf_typ  = make_aggregate agg kind;
          caf_off  = -1;
          caf_size = -1;
          caf_name = name;
        } in

        loop (field :: fields) tl

    | { d = FieldDecl (ty, name, bitwidth, init) } :: tl ->
        if bitwidth <> None then
          Log.unimp "Bit fields not implemented";
        if init <> None then
          Log.unimp "Member initialisers not implemented";
        let field = {
          caf_typ  = c_type_of_type_loc ty;
          caf_off  = -1;
          caf_size = -1;
          caf_name = name;
        } in

        loop (field :: fields) tl

    | { d } :: tl ->
        print_endline (Show.show<decl_> d);
        Log.err "Only FieldDecls allowed within RecordDecl"
  in

  loop [] decls


let c_var_of_parm_decl = function
  | { d = ParmVarDecl (ty, name) } ->
      {
        cv_name     = name;
        cv_uid      = -1;
        cv_type     = c_type_of_type_loc ty;
        cv_volatile = Clang.Query.is_volatile_tloc ty.tl;
      }
  | _ -> Log.err "only ParmVarDecls allowed in function argument list"


let c_decl_of_decl { d_sloc = { loc_s_line }; d } =
  match d with
  | VarDecl (ty, name, init) ->
      if init <> None then
        Log.unimp "Unsupported: initialiser in declaration";
      loc_s_line, {
        cv_name     = name;
        cv_uid      = -1;
        cv_type     = c_type_of_type_loc ty;
        cv_volatile = Clang.Query.is_volatile_tloc ty.tl;
      }

  | EmptyDecl ->
      Log.err "empty declaration within function body"
  | FunctionDecl _ ->
      Log.err "local function declarations are not supported by memcad AST"
  | TypedefDecl (ty, name) ->
      Log.err "local typedefs are not supported by memcad AST"
  | EnumDecl (name, enumerators) ->
      Log.err "local enums are not supported by memcad AST"
  | RecordDecl (name, members) ->
      Log.err "local structs are not supported by memcad AST"

  | EnumConstantDecl    _ -> Log.err "EnumConstantDecl found in function"
  | FieldDecl           _ -> Log.err "FieldDecl found in function"
  | ParmVarDecl         _ -> Log.err "ParmVarDecl found in function"
  | TranslationUnitDecl _ -> Log.err "TranslationUnitDecl found in function"

  | UnimpDecl name -> Log.unimp "%s" name


let rec c_lvalk_of_expr prog env = function
  | DeclRefExpr name ->
      Clvar (lookup_var prog env name)

  | MemberExpr (({ e_type = ty } as base), member, is_arrow) ->
      (*print_endline (Show.show<ctyp> ty);*)
      let base =
        if is_arrow then
          {
            clk = Clderef (c_expr_of_expr prog env base);
            clt = c_type_of_type ty.t;
          }
        else
          c_lval_of_expr prog env base
      in
      Clfield (base, member)

  | ArraySubscriptExpr (base, index) ->
      Clindex (
        c_lval_of_expr prog env base,
        c_expr_of_expr prog env index
      )

  | UnaryOperator (UO_Deref, expr) ->
      Clderef (c_expr_of_expr prog env expr)

  | IntegerLiteral _ -> Log.unimp "lvalk IntegerLiteral"
  | CharacterLiteral _ -> Log.unimp "lvalk CharacterLiteral"
  | FloatingLiteral _ -> Log.unimp "lvalk FloatingLiteral"
  | StringLiteral _ -> Log.unimp "lvalk StringLiteral"
  | BinaryOperator _ -> Log.unimp "lvalk BinaryOperator"
  | UnaryOperator _ -> Log.unimp "lvalk UnaryOperator"

  | PredefinedExpr _ -> Log.unimp "lvalk PredefinedExpr"
  | ImplicitCastExpr _ -> Log.unimp "lvalk ImplicitCastExpr"
  | CStyleCastExpr _ -> Log.unimp "lvalk CStyleCastExpr"
  | CompoundLiteralExpr _ -> Log.unimp "lvalk CompoundLiteralExpr"
  | ParenExpr _ -> Log.unimp "lvalk ParenExpr"
  | VAArgExpr _ -> Log.unimp "lvalk VAArgExpr"
  | CallExpr _ -> Log.unimp "lvalk CallExpr"
  | ConditionalOperator _ -> Log.unimp "lvalk ConditionalOperator"
  | DesignatedInitExpr _ -> Log.unimp "lvalk DesignatedInitExpr"
  | InitListExpr _ -> Log.unimp "lvalk InitListExpr"
  | ImplicitValueInitExpr -> Log.unimp "lvalk ImplicitValueInitExpr"
  | StmtExpr _ -> Log.unimp "lvalk StmtExpr"

  | SizeOfExpr _ -> Log.unimp "lvalk SizeOfExpr"
  | SizeOfType _ -> Log.unimp "lvalk SizeOfType"
  | AlignOfExpr _ -> Log.unimp "lvalk AlignOfExpr"
  | AlignOfType _ -> Log.unimp "lvalk AlignOfType"
  | VecStepExpr _ -> Log.unimp "lvalk VecStepExpr"
  | VecStepType _ -> Log.unimp "lvalk VecStepType"

  | UnimpExpr name -> Log.unimp "%s" name


and c_lval_of_expr prog env { e = expr; e_type; } =
  (*let canon = Clang.Api.(request @@ CanonicalType e_type.t_cref) in*)
  (*print_endline (Show.show<ctyp> canon);*)
  {
    clk = c_lvalk_of_expr prog env expr;
    clt = c_type_of_type e_type.t;
  }


and c_exprk_of_expr prog env = function
  | ImplicitCastExpr (_, expr) ->
      (* Ignore implicit casts within sub-expressions. *)
      c_exprk_of_expr prog env expr.e

  | IntegerLiteral i ->
      Ceconst (Ccint i)

  | BinaryOperator (op, lhs, rhs) ->
      Cebin (
        c_binop_of_binary_operator op,
        c_expr_of_expr prog env lhs,
        c_expr_of_expr prog env rhs
      )

  | UnaryOperator (UO_AddrOf, expr) ->
      Ceaddrof (
        c_lval_of_expr prog env expr
      )

  | UnaryOperator (op, expr) ->
      Ceuni (
        c_uniop_of_unary_operator op,
        c_expr_of_expr prog env expr
      )

  | ParenExpr expr ->
      c_exprk_of_expr prog env expr.e

  | CStyleCastExpr _ -> Log.unimp "exprk CStyleCastExpr"

  | CharacterLiteral _ -> Log.unimp "exprk CharacterLiteral"
  | FloatingLiteral _ -> Log.unimp "exprk FloatingLiteral"
  | StringLiteral _ -> Log.unimp "exprk StringLiteral"

  | PredefinedExpr _ -> Log.unimp "exprk PredefinedExpr"
  | CompoundLiteralExpr _ -> Log.unimp "exprk CompoundLiteralExpr"
  | VAArgExpr _ -> Log.unimp "exprk VAArgExpr"
  | CallExpr _ -> Log.unimp "exprk CallExpr"
  | ConditionalOperator _ -> Log.unimp "exprk ConditionalOperator"
  | DesignatedInitExpr _ -> Log.unimp "exprk DesignatedInitExpr"
  | InitListExpr _ -> Log.unimp "exprk InitListExpr"
  | ImplicitValueInitExpr -> Log.unimp "exprk ImplicitValueInitExpr"
  | StmtExpr _ -> Log.unimp "exprk StmtExpr"

  | SizeOfExpr _ -> Log.unimp "exprk SizeOfExpr"
  | SizeOfType _ -> Log.unimp "exprk SizeOfType"
  | AlignOfExpr _ -> Log.unimp "exprk AlignOfExpr"
  | AlignOfType _ -> Log.unimp "exprk AlignOfType"
  | VecStepExpr _ -> Log.unimp "exprk VecStepExpr"
  | VecStepType _ -> Log.unimp "exprk VecStepType"

  | UnimpExpr name -> Log.unimp "%s" name

  (* Already handled below. *)
  | ArraySubscriptExpr _ -> Log.err "exprk ArraySubscriptExpr"
  | MemberExpr _ -> Log.err "exprk MemberExpr"
  | DeclRefExpr _ -> Log.err "exprk DeclRefExpr"


and c_expr_of_expr prog env expr =
  match expr.e with
  (* (( void * )0) => null *)
  | CStyleCastExpr (
      _,
      { tl = PointerTypeLoc { tl = BuiltinTypeLoc BT_Void } },
      { e = IntegerLiteral 0 }
    ) ->
      {
        cek = Ceconst Ccnull;
        (* TODO: We could get the type of the outer ImplicitCastExpr. *)
        cet = Ctptr None;
      }

  | UnaryOperator (UO_Deref, _)
  | ArraySubscriptExpr _
  | MemberExpr _
  | DeclRefExpr _ ->
      (*print_endline (Show.show<expr> expr);*)
      (*print_endline (Show.show<ctyp> ty);*)
      {
        cek = Celval (c_lval_of_expr prog env expr);
        cet = c_type_of_type expr.e_type.t;
      }

  | _ ->
      (*print_endline (Show.show<expr> expr);*)
      {
        cek = c_exprk_of_expr prog env expr.e;
        cet = c_type_of_type expr.e_type.t;
      }


let make_call prog env callee args =
  {
    cc_fun = c_expr_of_expr prog env callee;
    cc_args = List.map (c_expr_of_expr prog env) args;
  }


let rec c_stat_of_expr prog env expr =
  match expr.e with
  | BinaryOperator (BO_Assign, lhs, rhs) ->
      (*print_endline (Show.show<expr> rhs);*)
      {
        csl = expr.e_sloc.loc_s_line;
        csk = Csassign (
          c_lval_of_expr prog env lhs,
          c_expr_of_expr prog env rhs
        );
      }

  | CallExpr (callee, args) ->
      {
        csl = expr.e_sloc.loc_s_line;
        csk = 
          (* Special handling for known functions. Note that
             malloc is not handled here, since it is only used
             within an assignment expression statement. *)
          match Clang.Query.identifier_of_expr callee.e, args with
          | "_memcad", [{ e = StringLiteral str }] ->
              Cs_memcad (Mc_comstring str)
          | "assert", [_] ->
              Csassert (c_expr_of_expr prog env (List.hd args))
          | "free", [_] ->
              Csfree (c_lval_of_expr prog env (List.hd args))
          | _ ->
              Cspcall (make_call prog env callee args)
      }

  | IntegerLiteral _ -> Log.unimp "stats IntegerLiteral"
  | CharacterLiteral _ -> Log.unimp "stats CharacterLiteral"
  | FloatingLiteral _ -> Log.unimp "stats FloatingLiteral"
  | StringLiteral _ -> Log.unimp "stats StringLiteral"
  | BinaryOperator _ -> Log.unimp "stats BinaryOperator"
  | UnaryOperator _ -> Log.unimp "stats UnaryOperator"

  | DeclRefExpr _ -> Log.unimp "stats DeclRefExpr"
  | PredefinedExpr _ -> Log.unimp "stats PredefinedExpr"
  | ImplicitCastExpr _ -> Log.unimp "stats ImplicitCastExpr"
  | CStyleCastExpr _ -> Log.unimp "stats CStyleCastExpr"
  | CompoundLiteralExpr _ -> Log.unimp "stats CompoundLiteralExpr"
  | ParenExpr _ -> Log.unimp "stats ParenExpr"
  | VAArgExpr _ -> Log.unimp "stats VAArgExpr"
  | MemberExpr _ -> Log.unimp "stats MemberExpr"
  | ConditionalOperator _ -> Log.unimp "stats ConditionalOperator"
  | DesignatedInitExpr _ -> Log.unimp "stats DesignatedInitExpr"
  | InitListExpr _ -> Log.unimp "stats InitListExpr"
  | ImplicitValueInitExpr -> Log.unimp "stats ImplicitValueInitExpr"
  | ArraySubscriptExpr _ -> Log.unimp "stats ArraySubscriptExpr"
  | StmtExpr _ -> Log.unimp "stats StmtExpr"

  | SizeOfExpr _ -> Log.unimp "stats SizeOfExpr"
  | SizeOfType _ -> Log.unimp "stats SizeOfType"
  | AlignOfExpr _ -> Log.unimp "stats AlignOfExpr"
  | AlignOfType _ -> Log.unimp "stats AlignOfType"
  | VecStepExpr _ -> Log.unimp "stats VecStepExpr"
  | VecStepType _ -> Log.unimp "stats VecStepType"

  | UnimpExpr name -> Log.unimp "%s" name


(* This function maps N clang statements to M memcad statements.
   M may be considerably more than N. *)
let rec c_stats_of_stmts prog env stmts =
  let rec loop env stats = function
    | [] -> stats

    | stmt :: tl ->
        match stmt.s with
        | ReturnStmt expr ->
            let stat = {
              csl = stmt.s_sloc.loc_s_line;
              csk = Csreturn (
                match expr with
                | None ->
                    None
                | Some expr ->
                    Some (c_expr_of_expr prog env (Clang.ImplicitCast.strip_expr expr))
              );
            } in
            loop env (stat :: stats) tl

        | ExprStmt e ->
            let stat =
              match Clang.ImplicitCast.strip_expr e with
              (* Special handling of malloc. *)
              | { e = BinaryOperator (BO_Assign, lhs,
                                      { e = CallExpr (callee, [arg]) })
                } when Clang.Query.identifier_of_expr callee.e = "malloc" ->
                  {
                    csl = stmt.s_sloc.loc_s_line;
                    csk = Csalloc (
                      c_lval_of_expr prog env lhs,
                      c_expr_of_expr prog env arg
                    );
                  }

              (* Special handling of assigned call expressions. *)
              | { e = BinaryOperator (BO_Assign, lhs,
                                      { e = CallExpr (callee, args) })
                } ->
                  {
                    csl = stmt.s_sloc.loc_s_line;
                    csk = Csfcall (
                      c_lval_of_expr prog env lhs,
                      make_call prog env callee args
                    );
                  }

              | e ->
                  (*print_endline (Show.show<expr> e);*)
                  c_stat_of_expr prog env e
            in
            loop env (stat :: stats) tl

        (* We only accept single declarations. *)
        | DeclStmt [decl] ->
            let (loc, c_decl) = c_decl_of_decl decl in
            let env = StringMap.add c_decl.cv_name c_decl env in
            let stat = { csl = loc; csk = Csdecl c_decl } in
            loop env (stat :: stats) tl

        | WhileStmt (cond, body) ->
            let cond = Clang.ImplicitCast.strip_expr cond in
            let stat =
              let stmts = Clang.Query.body_of_stmt body in
              {
                csl = stmt.s_sloc.loc_s_line;
                csk = Cswhile (
                  c_expr_of_expr prog env cond,
                  c_stats_of_stmts prog env stmts,
                  None
                );
              }
            in
            loop env (stat :: stats) tl

        | IfStmt (cond, then_body, else_body) ->
            let cond = Clang.ImplicitCast.strip_expr cond in
            let stat =
              let then_stmts = Clang.Query.body_of_stmt then_body in
              let else_stmts =
                match else_body with
                | None -> []
                | Some else_body -> Clang.Query.body_of_stmt else_body
              in
              {
                csl = stmt.s_sloc.loc_s_line;
                csk = Csif (
                  c_expr_of_expr prog env cond,
                  c_stats_of_stmts prog env then_stmts,
                  c_stats_of_stmts prog env else_stmts
                );
              }
            in
            loop env (stat :: stats) tl

        | BreakStmt ->
            let stat = {
              csl = stmt.s_sloc.loc_s_line;
              csk = Csbreak;
            } in
            loop env (stat :: stats) tl

        | CompoundStmt stmts ->
            let stat = {
              csl = stmt.s_sloc.loc_s_line;
              csk = Csblock
                (c_stats_of_stmts prog env stmts)
            } in
            loop env (stat :: stats) tl

        | NullStmt -> Log.unimp "NullStmt"
        | ContinueStmt -> Log.unimp "ContinueStmt"
        | LabelStmt _ -> Log.unimp "LabelStmt"
        | CaseStmt _ -> Log.unimp "CaseStmt"
        | DefaultStmt _ -> Log.unimp "DefaultStmt"
        | GotoStmt _ -> Log.unimp "GotoStmt"
        | ForStmt _ -> Log.unimp "ForStmt"
        | DoStmt _ -> Log.unimp "DoStmt"
        | SwitchStmt _ -> Log.unimp "SwitchStmt"
        | DeclStmt _ -> Log.unimp "DeclStmt"

        | UnimpStmt name -> Log.unimp "%s" name
  in
  (* We build the list in reverse. *)
  List.rev (loop env [] stmts)


let make_env = function
  | FunctionProtoTypeLoc (_, args) ->
      List.fold_left (fun env decl ->
        let parm = c_var_of_parm_decl decl in
        StringMap.add parm.cv_name parm env
      ) StringMap.empty args

  | FunctionNoProtoTypeLoc (_) ->
      StringMap.empty

  | tl -> failwith (Show.show<tloc_> tl)


let c_fun_body_of_stmts prog ty body =
  let stmts = Clang.Query.body_of_stmt body in
  c_stats_of_stmts prog (make_env ty.tl) stmts


let rec collect_decls prog = function
  | [] -> prog

  | { d = EmptyDecl } :: tl ->
      collect_decls prog tl

  | { d = FunctionDecl (_, _, None) } :: tl ->
      (* Function declarations (without definition) do nothing. *)
      collect_decls prog tl

  | { d = FunctionDecl (ty, name, Some body) } :: tl ->
      let c_fun =
        (* Create the head of the function (without body), first,
           so that name lookups within the body work for the
           currently processed function name. *)
        let stub = {
          cf_type = c_type_of_type_loc (Clang.Query.return_type_of_tloc ty.tl);
          cf_uid  = -1;
          cf_name = name;
          cf_args = List.map c_var_of_parm_decl (Clang.Query.args_of_tloc ty.tl);
          cf_body = []; (* Filled in later. *)
        } in
        let prog = add_fun name stub prog in
        { stub with cf_body = c_fun_body_of_stmts prog ty body }
      in
      collect_decls (add_fun name c_fun prog) tl

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
    :: { d = TypedefDecl (
        { tl = ElaboratedTypeLoc {
             tl = RecordTypeLoc (kind, name2)
           }
        },
        name)
      }
    :: tl
    when name1 = name2 ->
      let c_type =
        make_aggregate {
          cag_name   = if name1 = "" then None else Some name1;
          cag_align  = -1;
          cag_size   = -1;
          cag_fields = c_agg_fields_of_decls members;
        } kind
      in
      collect_decls (add_type name c_type prog) tl

  (* Handle "typedef struct foo *Foo;" (where struct foo was not
     yet defined) specially, as well. *)
  |   { d = RecordDecl (name1, members) }
    :: { d = TypedefDecl (
        { tl = PointerTypeLoc
          { tl = ElaboratedTypeLoc {
               tl = RecordTypeLoc (_, name2)
             }
          }
        } as ty,
        name)
      }
    :: tl
    when name1 = name2 ->
      let c_type = c_type_of_type_loc ty in
      collect_decls (add_type name c_type prog) tl

  (* There may be other typedefs involving a preceding record definition,
     so this case is printed with a better diagnostic than the catch-all
     case below. *)
  |   { d = RecordDecl (name, members) }
    :: { d = TypedefDecl _ as tdef }
    :: tl ->
      Log.unimp "unsupported record/typedef combination: %a"
        Show.format<decl_> tdef

  (* Any lone RecordDecls are not supported. *)
  | { d = RecordDecl (name, members) } :: tl ->
      Log.unimp "RecordDecl without TypedefDecl not supported by memcad AST"

  | { d = TypedefDecl (ty, name) } :: tl ->
      let c_type = c_type_of_type_loc ty in
      collect_decls (add_type name c_type prog) tl

  | { d = VarDecl (ty, name, init) } as decl :: tl ->
      let c_var = snd (c_decl_of_decl decl) in
      collect_decls (add_var name c_var prog) tl

  | { d = EnumDecl (name, enumerators) } :: tl ->
      (* TODO *)
      collect_decls prog tl

  | { d = EnumConstantDecl    _ } :: _ -> Log.err "EnumConstantDecl found at file scope"
  | { d = FieldDecl           _ } :: _ -> Log.err "FieldDecl found at file scope"
  | { d = ParmVarDecl         _ } :: _ -> Log.err "ParmVarDecl found at file scope"
  | { d = TranslationUnitDecl _ } :: _ -> Log.err "nested TranslationUnitDecl found"

  | { d = UnimpDecl name } :: _ -> Log.unimp "%s" name


let c_prog_from_decl = function
  | { d = TranslationUnitDecl decls } ->
      collect_decls C_utils.empty_unit decls
  | _ -> Log.err "c_prog_from_decl requires a translation unit"
