open C_sig
open Clang
open Ast
open Data_structures
open Util

module Log = Util.Logger.Make(struct let tag = "transform" end)


type env = {
  prog : c_prog;
  ns   : string;
}


let start_line clang loc =
  Api.(request clang @@ PresumedLoc loc.loc_s).Sloc.loc_line - 1


let sizeof clang ctyp =
  let open Api in
  request clang @@ SizeofType ctyp.t_cref
  |> Int64.to_int


let alignof clang ctyp =
  let open Api in
  request clang @@ AlignofType ctyp.t_cref


let lookup_var env decl_env name =
  try
    StringMap.find name decl_env
  with Not_found ->
    try
      StringMap.find name env.prog.cp_vars
    with Not_found ->
      try
        let func = StringMap.find name env.prog.cp_funs in
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
    Pp.pp_stmt s

let dump_expr s =
  Format.printf "%a\n"
    Pp.pp_expr s


let add_type name ty env =
  match name with
  | "__int128_t"
  | "__uint128_t"
  | "__builtin_va_list" -> env
  | name ->
      let name = env.ns ^ name in
      assert (not (StringMap.mem name env.prog.cp_types));
      { env with prog = {
           env.prog with cp_types = StringMap.add name ty env.prog.cp_types }
      }

let add_fun name fn env =
  let name = env.ns ^ name in
  assert (not (StringMap.mem name env.prog.cp_funs));
  { env with prog = {
       env.prog with cp_funs = StringMap.add name fn env.prog.cp_funs }
  }

let add_var name var env =
  let name = env.ns ^ name in
  assert (not (StringMap.mem name env.prog.cp_vars));
  { env with prog = {
       env.prog with cp_vars = StringMap.add name var env.prog.cp_vars }
  }


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


let c_var_of_parm_decl c_types = function
  | { d = ParmVarDecl (ty, name) } ->
      {
        cv_name     = name;
        cv_uid      = -1;
        cv_type     = DenseIntMap.find ty.tl_type.t_self c_types;
        cv_volatile = Query.is_volatile_tloc ty.tl;
      }
  | _ -> Log.err "only ParmVarDecls allowed in function argument list"


let c_decl_of_decl clang c_types { d_sloc; d } =
  let loc_s_line = start_line clang d_sloc in

  match d with
  | VarDecl (ty, name, init) ->
      if init <> None then
        Log.unimp "Unsupported: initialiser in declaration";
      loc_s_line, {
        cv_name     = name;
        cv_uid      = -1;
        cv_type     = DenseIntMap.find ty.tl_type.t_self c_types;
        cv_volatile = Query.is_volatile_tloc ty.tl;
      }

  | EmptyDecl ->
      Log.err "empty declaration within function body"
  | FunctionDecl _ ->
      Log.err "local function declarations are not supported by memcad AST"
  | TypedefDecl (ty, name) ->
      Log.err "local typedefs are not supported by memcad AST"
  | EnumDecl (name, enumerators) ->
      Log.err "local enums are not supported by memcad AST"
  | RecordDecl (kind, name, members, _) ->
      Log.err "local %ss are not supported by memcad AST"
        (Pp.string_of_tag_type_kind kind)

  | EnumConstantDecl    _ -> Log.err "EnumConstantDecl found in function"
  | FieldDecl           _ -> Log.err "FieldDecl found in function"
  | ParmVarDecl         _ -> Log.err "ParmVarDecl found in function"
  | TranslationUnitDecl _ -> Log.err "TranslationUnitDecl found in function"

  | decl -> Log.unimp "%a" Show.format<decl_> decl


let rec c_lvalk_of_expr c_types env decl_env = function
  | DeclRefExpr name ->
      Clvar (lookup_var env decl_env name)

  | MemberExpr (({ e_type = ty } as base), member, is_arrow) ->
      (*print_endline (Show.show<ctyp> ty);*)
      let base =
        if is_arrow then
          {
            clk = Clderef (c_expr_of_expr c_types env decl_env base);
            clt = DenseIntMap.find ty.t_self c_types;
          }
        else
          c_lval_of_expr c_types env decl_env base
      in
      Clfield (base, member)

  | ArraySubscriptExpr (base, index) ->
      Clindex (
        c_lval_of_expr c_types env decl_env base,
        c_expr_of_expr c_types env decl_env index
      )

  | UnaryOperator (UO_Deref, expr) ->
      Clderef (c_expr_of_expr c_types env decl_env expr)

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
  | AddrLabelExpr _ -> Log.unimp "lvalk AddrLabelExpr"

  | SizeOfExpr _ -> Log.unimp "lvalk SizeOfExpr"
  | SizeOfType _ -> Log.unimp "lvalk SizeOfType"
  | AlignOfExpr _ -> Log.unimp "lvalk AlignOfExpr"
  | AlignOfType _ -> Log.unimp "lvalk AlignOfType"
  | VecStepExpr _ -> Log.unimp "lvalk VecStepExpr"
  | VecStepType _ -> Log.unimp "lvalk VecStepType"

  | expr -> Log.unimp "%a" Show.format<expr_> expr


and c_lval_of_expr c_types env decl_env { e = expr; e_type; } =
  {
    clk = c_lvalk_of_expr c_types env decl_env expr;
    clt = DenseIntMap.find e_type.t_self c_types;
  }


and c_exprk_of_expr c_types env decl_env = function
  | IntegerLiteral i ->
      Ceconst (Ccint i)

  | BinaryOperator (op, lhs, rhs) ->
      Cebin (
        c_binop_of_binary_operator op,
        c_expr_of_expr c_types env decl_env lhs,
        c_expr_of_expr c_types env decl_env rhs
      )

  | UnaryOperator (UO_AddrOf, expr) ->
      Ceaddrof (
        c_lval_of_expr c_types env decl_env expr
      )

  | UnaryOperator (op, expr) ->
      Ceuni (
        c_uniop_of_unary_operator op,
        c_expr_of_expr c_types env decl_env expr
      )

  | ParenExpr expr ->
      c_exprk_of_expr c_types env decl_env expr.e

  | CStyleCastExpr _ -> Log.unimp "exprk CStyleCastExpr"
  | ImplicitCastExpr _ -> Log.unimp "exprk ImplicitCastExpr"

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
  | AddrLabelExpr _ -> Log.unimp "exprk AddrLabelExpr"

  | SizeOfExpr _ -> Log.unimp "exprk SizeOfExpr"
  | SizeOfType _ -> Log.unimp "exprk SizeOfType"
  | AlignOfExpr _ -> Log.unimp "exprk AlignOfExpr"
  | AlignOfType _ -> Log.unimp "exprk AlignOfType"
  | VecStepExpr _ -> Log.unimp "exprk VecStepExpr"
  | VecStepType _ -> Log.unimp "exprk VecStepType"

  (* Already handled below. *)
  | ArraySubscriptExpr _ -> Log.err "exprk ArraySubscriptExpr"
  | MemberExpr _ -> Log.err "exprk MemberExpr"
  | DeclRefExpr _ -> Log.err "exprk DeclRefExpr"

  | expr -> Log.unimp "exprk %a" Show.format<expr_> expr


and c_expr_of_expr c_types env decl_env expr =
  match expr.e with
  (* (( void * )0) => null *)
  | CStyleCastExpr (
      _,
      { tl = PointerTypeLoc { tl = BuiltinTypeLoc BT_Void } },
      { e = IntegerLiteral 0 }
    ) ->
      {
        cek = Ceconst Ccnull;
        cet = DenseIntMap.find expr.e_type.t_self c_types;
      }

  | UnaryOperator (UO_Deref, _)
  | ArraySubscriptExpr _
  | MemberExpr _
  | DeclRefExpr _ ->
      (*print_endline (Show.show<expr> expr);*)
      (*print_endline (Show.show<ctyp> ty);*)
      {
        cek = Celval (c_lval_of_expr c_types env decl_env expr);
        cet = DenseIntMap.find expr.e_type.t_self c_types;
      }

  | _ ->
      (*print_endline (Show.show<expr> expr);*)
      {
        cek = c_exprk_of_expr c_types env decl_env expr.e;
        cet = DenseIntMap.find expr.e_type.t_self c_types;
      }


let make_call c_types env decl_env callee args =
  {
    cc_fun = c_expr_of_expr c_types env decl_env callee;
    cc_args = List.map (c_expr_of_expr c_types env decl_env) args;
  }


let rec c_stat_of_expr clang c_types env decl_env expr =
  match expr.e with
  | BinaryOperator (BO_Assign, lhs, rhs) ->
      (*print_endline (Show.show<expr> rhs);*)
      {
        csl = start_line clang expr.e_sloc;
        csk = Csassign (
          c_lval_of_expr c_types env decl_env lhs,
          c_expr_of_expr c_types env decl_env rhs
        );
      }

  | CallExpr (callee, args) ->
      {
        csl = start_line clang expr.e_sloc;
        csk =
          (* Special handling for known functions. Note that
             malloc is not handled here, since it is only used
             within an assignment expression statement. *)
          match Query.identifier_of_expr callee.e, args with
          | "_memcad", [{ e = StringLiteral str }] ->
              Cs_memcad (Mc_comstring str)
          | "assert", [_] ->
              Csassert (c_expr_of_expr c_types env decl_env (List.hd args))
          | "free", [_] ->
              Csfree (c_lval_of_expr c_types env decl_env (List.hd args))
          | _ ->
              Cspcall (make_call c_types env decl_env callee args)
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
  | AddrLabelExpr _ -> Log.unimp "stats AddrLabelExpr"

  | SizeOfExpr _ -> Log.unimp "stats SizeOfExpr"
  | SizeOfType _ -> Log.unimp "stats SizeOfType"
  | AlignOfExpr _ -> Log.unimp "stats AlignOfExpr"
  | AlignOfType _ -> Log.unimp "stats AlignOfType"
  | VecStepExpr _ -> Log.unimp "stats VecStepExpr"
  | VecStepType _ -> Log.unimp "stats VecStepType"

  | expr -> Log.unimp "stats %a" Show.format<expr_> expr


(* This function maps N clang statements to M memcad statements.
   M may be considerably more than N. *)
let rec c_stats_of_stmts clang c_types env decl_env stmts =
  let rec loop decl_env stats = function
    | [] -> stats

    | stmt :: tl ->
        let csl = start_line clang stmt.s_sloc in

        match stmt.s with
        | ReturnStmt expr ->
            let stat = {
              csl;
              csk = Csreturn (
                match expr with
                | None ->
                    None
                | Some expr ->
                    Some (c_expr_of_expr c_types env decl_env expr)
              );
            } in
            loop decl_env (stat :: stats) tl

        | ExprStmt e ->
            let stat =
              match e with
              (* Special handling of malloc. *)
              | { e = BinaryOperator (BO_Assign, lhs,
                                      { e = CallExpr (callee, [arg]) })
                } when Query.identifier_of_expr callee.e = "malloc" ->
                  {
                    csl;
                    csk = Csalloc (
                      c_lval_of_expr c_types env decl_env lhs,
                      c_expr_of_expr c_types env decl_env arg
                    );
                  }

              (* Special handling of assigned call expressions. *)
              | { e = BinaryOperator (BO_Assign, lhs,
                                      { e = CallExpr (callee, args) })
                } ->
                  {
                    csl;
                    csk = Csfcall (
                      c_lval_of_expr c_types env decl_env lhs,
                      make_call c_types env decl_env callee args
                    );
                  }

              | e ->
                  (*print_endline (Show.show<expr> e);*)
                  c_stat_of_expr clang c_types env decl_env e
            in
            loop decl_env (stat :: stats) tl

        (* We only accept single declarations. *)
        | DeclStmt [decl] ->
            let (loc, c_decl) = c_decl_of_decl clang c_types decl in
            let decl_env = StringMap.add c_decl.cv_name c_decl decl_env in
            let stat = { csl = loc; csk = Csdecl c_decl } in
            loop decl_env (stat :: stats) tl

        | WhileStmt (cond, body) ->
            let stat =
              let stmts = Query.body_of_stmt body in
              {
                csl;
                csk = Cswhile (
                  c_expr_of_expr c_types env decl_env cond,
                  c_stats_of_stmts clang c_types env decl_env stmts,
                  None
                );
              }
            in
            loop decl_env (stat :: stats) tl

        | IfStmt (cond, then_body, else_body) ->
            let stat =
              let then_stmts = Query.body_of_stmt then_body in
              let else_stmts =
                match else_body with
                | None -> []
                | Some else_body -> Query.body_of_stmt else_body
              in
              {
                csl;
                csk = Csif (
                  c_expr_of_expr c_types env decl_env cond,
                  c_stats_of_stmts clang c_types env decl_env then_stmts,
                  c_stats_of_stmts clang c_types env decl_env else_stmts
                );
              }
            in
            loop decl_env (stat :: stats) tl

        | BreakStmt ->
            let stat = {
              csl;
              csk = Csbreak;
            } in
            loop decl_env (stat :: stats) tl

        | CompoundStmt stmts ->
            let stat = {
              csl;
              csk = Csblock
                (c_stats_of_stmts clang c_types env decl_env stmts)
            } in
            loop decl_env (stat :: stats) tl

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

        | stmt -> Log.unimp "%a" Show.format<stmt_> stmt
  in
  (* We build the list in reverse. *)
  List.rev (loop decl_env [] stmts)


let make_env c_types = function
  | FunctionProtoTypeLoc (_, args) ->
      List.fold_left (fun decl_env decl ->
        let parm = c_var_of_parm_decl c_types decl in
        StringMap.add parm.cv_name parm decl_env
      ) StringMap.empty args

  | FunctionNoProtoTypeLoc (_) ->
      StringMap.empty

  | tl -> failwith (Show.show<tloc_> tl)


let c_fun_body_of_stmts clang c_types env ty body =
  let stmts = Query.body_of_stmt body in
  c_stats_of_stmts clang c_types env (make_env c_types ty.tl) stmts


let rec collect_decls clang c_types env = function
  | [] -> env

  | { d = EmptyDecl } :: tl ->
      collect_decls clang c_types env tl

  | { d = FunctionDecl (_, _, None) } :: tl ->
      (* Function declarations (without definition) do nothing. *)
      collect_decls clang c_types env tl

  | { d = FunctionDecl (ty, DN_Identifier name, Some body) } :: tl ->
      let c_fun =
        (* Create the head of the function (without body), first,
           so that name lookups within the body work for the
           currently processed function name. *)
        let stub = {
          cf_type = DenseIntMap.find (Query.return_type_of_tloc ty.tl).tl_type.t_self c_types;
          cf_uid  = -1;
          cf_name = name;
          cf_args = List.map (c_var_of_parm_decl c_types) (Query.args_of_tloc ty.tl);
          cf_body = []; (* Filled in later. *)
        } in
        let env = add_fun name stub env in
        { stub with cf_body = c_fun_body_of_stmts clang c_types env ty body }
      in
      collect_decls clang c_types (add_fun name c_fun env) tl

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
  |   { d = RecordDecl (_, name1, Some members, _) }
    :: { d = TypedefDecl (
        { tl = ElaboratedTypeLoc {
            tl = RecordTypeLoc (kind, name2);
            tl_type;
          };
        },
        name)
      }
    :: tl
    when name1 = name2 ->
      let c_type = DenseIntMap.find tl_type.t_self c_types in
      collect_decls clang c_types (add_type name c_type env) tl

  (* Handle "typedef struct foo *Foo;" (where struct foo was not
     yet defined) specially, as well. *)
  |   { d = RecordDecl (_, name1, _, _) }
    :: { d = TypedefDecl (
        { tl = (
            PointerTypeLoc
              { tl = ElaboratedTypeLoc {
                  tl = RecordTypeLoc (_, name2);
                  tl_type;
                }
              }
          | ElaboratedTypeLoc {
              tl = RecordTypeLoc (_, name2);
              tl_type;
            }
          )
        },
        name)
      }
    :: tl
    when name1 = name2 ->
      let c_type = DenseIntMap.find tl_type.t_self c_types in
      collect_decls clang c_types (add_type name c_type env) tl

  (* There may be other typedefs involving a preceding record definition,
     so this case is printed with a better diagnostic than the catch-all
     case below. *)
  |   { d = RecordDecl (_, name, members, _) as decl }
    :: { d = TypedefDecl _ as tdef }
    :: tl ->
      Log.unimp "unsupported record/typedef combination: [%a;\n%a;]"
        Show.format<decl_> decl
        Show.format<decl_> tdef

  (* Any lone RecordDecls are not supported. *)
  | { d = RecordDecl (_, name, members, _) } :: tl ->
      Log.unimp "RecordDecl without TypedefDecl not supported by memcad AST"

  | { d = TypedefDecl (ty, name) } :: tl ->
      let c_type = DenseIntMap.find ty.tl_type.t_self c_types in
      collect_decls clang c_types (add_type name c_type env) tl

  | { d = VarDecl (ty, name, init) } as decl :: tl ->
      let c_var = snd (c_decl_of_decl clang c_types decl) in
      collect_decls clang c_types (add_var name c_var env) tl

  | { d = EnumDecl (name, enumerators) } :: tl ->
      (* TODO *)
      collect_decls clang c_types env tl

  | { d = LinkageSpecDecl (decls, lang) } :: tl ->
      collect_decls clang c_types (collect_decls clang c_types env decls) tl

  | { d = NamespaceDecl (name, inline, decls) } :: tl ->
      let env = {
        env with ns = env.ns ^ name ^ "::"
      } in
      collect_decls clang c_types (collect_decls clang c_types env decls) tl

  | { d = EnumConstantDecl    _ } :: _ -> Log.err "EnumConstantDecl found at file scope"
  | { d = FieldDecl           _ } :: _ -> Log.err "FieldDecl found at file scope"
  | { d = ParmVarDecl         _ } :: _ -> Log.err "ParmVarDecl found at file scope"
  | { d = TranslationUnitDecl _ } :: _ -> Log.err "nested TranslationUnitDecl found"

  | { d } :: _ -> Log.unimp "%a" Show.format<decl_> d


let c_prog_from_decl clang = function
  | { d = TranslationUnitDecl decls } ->
      let types = Api.(request clang @@ CacheFor Cache_ctyp) in
      let c_types = CtypToMCType.map_types clang types in

      C_utils.max_c_var_id := 0;

      let env = {
        prog = C_utils.empty_unit;
        ns   = "";
      } in
      (collect_decls clang c_types env decls).prog
      (*|> C_process.c_prog_fix_types*)
      |> C_process.bind_c_prog

  | _ -> Log.err "c_prog_from_decl requires a translation unit"
