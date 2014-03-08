open Clang
open Prelude


type state = {
  (* We keep the decls and stmts separate, so we can
     append only the stmts at the end of a loop, where
     we need to insert them twice (once before the loop
     and once after each iteration). *)
  inserted_decls : Ast.stmt list;
  inserted_stmts : Ast.stmt list;
  (* We keep track of the number of temporaries created.
     When entering a compound statement, the state is
     duplicated, so two consequtive compound statements
     can have the same temporary variable names, but no
     shadowing can occur. *)
  generated_vars : int;
}

let empty_state = {
  inserted_decls = [];
  inserted_stmts = [];
  generated_vars = 0;
}


(* Clear inserted decls and stmts. *)
let clear_state state = {
  state with
  inserted_decls = [];
  inserted_stmts = [];
}


let make_temporary state =
  let var = "%tmp" ^ string_of_int state.generated_vars in
  ({ state with generated_vars = state.generated_vars + 1 }, var)


let assign var expr =
  let open Ast in

  ExprStmt {
    e = BinaryOperator (
        BO_Assign,
        { e = DeclRefExpr var;
          e_sloc = expr.e_sloc;
          e_type = expr.e_type;
          e_cref = Ref.null;
        },
        expr
      );
    e_sloc = expr.e_sloc;
    e_type = expr.e_type;
    e_cref = Ref.null;
  }


let append_stmts stmt stmts =
  let open Ast in

  match stmts with
  | [] -> stmt
  | stmts ->
      { s = CompoundStmt (stmt :: stmts);
        s_sloc = stmt.s_sloc;
        s_cref = Ref.null;
      }


let transform_decl clang =
  let open Ast in

  let rec map_expr v state expr =
    match expr.e with
    | ConditionalOperator (cond, then_expr, else_expr) ->
        let (state, var) = make_temporary state in

        let if_stmt = {
          s = IfStmt (
              cond,
              { s = assign var then_expr;
                s_sloc = then_expr.e_sloc;
                s_cref = Ref.null;
              },
              Some { s = assign var else_expr;
                     s_sloc = else_expr.e_sloc;
                     s_cref = Ref.null;
                   }
            );
          s_sloc = cond.e_sloc;
          s_cref = Ref.null;
        } in

        let var_decl = {
          s = DeclStmt [{
              d = VarDecl (
                  Types.tloc_of_ctyp expr.e_sloc expr.e_type,
                  var,
                  None
                );
              d_sloc = cond.e_sloc;
              d_cref = Ref.null;
            }];
          s_sloc = cond.e_sloc;
          s_cref = Ref.null;
        } in

        let (state, if_stmt) = map_stmt v state if_stmt in

        let inserted_stmts = if_stmt :: state.inserted_stmts in
        let inserted_decls = var_decl :: state.inserted_decls in

        let e = DeclRefExpr var in

        ({ state with inserted_stmts; inserted_decls },
         { expr with e })

    | _ ->
        MapVisitor.visit_expr v state expr


  and map_stmt v state stmt =
    match stmt.s with
    | CompoundStmt stmts ->
        (* There should be no unclaimed inserted decls/stmts. *)
        assert (state.inserted_decls == []);
        assert (state.inserted_stmts == []);

        (* We drop the state after the compound statement,
           so we can reuse temporary variable names. *)
        let _, stmts =
          List.fold_left (fun (state, stmts) stmt ->
            let (state, stmt) = map_stmt v state stmt in
            (clear_state state,
             stmt :: state.inserted_stmts @ state.inserted_decls @ stmts)
          ) (state, []) stmts
        in

        (state, { stmt with s = CompoundStmt (List.rev stmts) })

    | IfStmt (cond, then_stmt, else_stmt) ->
        let (state, cond) = map_expr v state cond in
        let then_stmt = map_sub_stmt v state then_stmt in
        let else_stmt = Option.map (map_sub_stmt v state) else_stmt in

        (state, { stmt with s = IfStmt (cond, then_stmt, else_stmt) })

    | WhileStmt (cond, body) ->
        let (state, cond) = map_expr v state cond in
        let body = map_sub_stmt v state body in

        (* Execute the replacement code for the condition again
           after each iteration. *)
        let body = append_stmts body state.inserted_stmts in

        (state, { stmt with s = WhileStmt (cond, body) })

    | _ ->
        MapVisitor.visit_stmt v state stmt


  (* This function is used for statements with sub-statements.
     If we want to insert more statements into the sub-statement,
     we wrap the original sub-statement together with the added
     statements and declarations in a new compound statement.
     If no statements were added, we leave it alone. *)
  and map_sub_stmt v state stmt =
    let (state, stmt) = map_stmt v (clear_state state) stmt in

    match state.inserted_stmts, state.inserted_decls with
    | [], [] -> stmt
    | stmts, decls ->
        { s = CompoundStmt (List.rev @@ stmt :: stmts @ decls);
          s_sloc = stmt.s_sloc;
          s_cref = Ref.null;
        }


  and map_decl v state decl =
    match decl.d with
    | EnumConstantDecl (name, value) ->
        (* These might contain constant conditionals. We should
           compute them here or in another constant computation
           pass. *)
        (state, decl)

    | _ ->
        MapVisitor.visit_decl v state decl


  in

  let v = MapVisitor.({
    default with
    map_decl;
    map_expr;
    map_stmt;
  }) in

  snd % MapVisitor.visit_decl v empty_state
