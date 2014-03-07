open Clang

let (%) f g = fun x -> f (g x)


let simplify_unit =
  let open Ast in

  let rec v = Visitor.({
    map_desg = (fun state desg -> visit_desg v state desg);
    map_decl = (fun state desg -> visit_decl v state desg);
    map_expr = (fun state desg -> visit_expr v state desg);
    map_ctyp = (fun state desg -> visit_ctyp v state desg);
    map_tloc = (fun state desg -> visit_tloc v state desg);
    map_stmt;
  })


  and map_stmt state stmt =
    match stmt.s with
    | CompoundStmt stmts ->
        let stmts =
          List.fold_left (fun stmts stmt ->
            let (state, stmt) = map_stmt state stmt in
            match state with
            | [] -> stmt :: stmts
            | xs -> xs @ stmts
          ) [] stmts
        in

        (state, { stmt with s = CompoundStmt (List.rev stmts) })

    | DeclStmt decls ->
        let replacement =
          List.rev_map (fun decl ->
            { stmt with
              s = DeclStmt [decl];
              s_sloc = decl.d_sloc;
            }
          ) decls
        in

        (replacement, stmt)

    | _ ->
        Visitor.visit_stmt v state stmt

  in

  snd % Visitor.visit_decl v []
