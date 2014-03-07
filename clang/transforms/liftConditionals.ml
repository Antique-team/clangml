open Clang
open Prelude


let transform_decl =
  let open Ast in

  let rec v = MapVisitor.({
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
        MapStmt.mapCompoundStmt map_stmt state stmt stmts

    | _ ->
        MapVisitor.visit_stmt v state stmt

  in

  snd % MapVisitor.visit_decl v []
