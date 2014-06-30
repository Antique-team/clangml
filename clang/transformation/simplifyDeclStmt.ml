open Clang
open Util.Prelude


let transform_decl clang =
  let open Ast in

  let rec map_stmt v state stmt =
    match stmt.s with
    | CompoundStmt stmts ->
        MapStmt.mapCompoundStmt map_stmt v state stmt stmts

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

  let v = MapVisitor.({ default with map_stmt }) in

  snd % MapVisitor.visit_decl v []
