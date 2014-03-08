open Clang
open Prelude


let transform_decl =
  let open Ast in

  let rec v = MapVisitor.({
    map_desg = (fun state desg -> visit_desg v state desg);
    map_decl = (fun state decl -> visit_decl v state decl);
    map_expr = (fun state expr -> visit_expr v state expr);
    map_ctyp = (fun state ctyp -> visit_ctyp v state ctyp);
    map_tloc = (fun state tloc -> visit_tloc v state tloc);
    map_stmt;
  })


  and map_stmt state stmt =
    match stmt.s with
    | CompoundStmt stmts ->
        MapStmt.mapCompoundStmt map_stmt state stmt stmts

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
        MapVisitor.visit_stmt v state stmt

  in

  snd % MapVisitor.visit_decl v []
