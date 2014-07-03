open Clang.Ast


let mapCompoundStmt ?(replace=true) map_stmt v state stmt stmts =
  let stmts =
    List.fold_left (fun stmts stmt ->
      let (state, stmt) = map_stmt v state stmt in
      match state with
      | [] -> stmt :: stmts
      | replacement ->
          if replace then
            replacement @ stmts
          else
            stmt :: replacement @ stmts
    ) [] stmts
  in

  (state, { stmt with s = CompoundStmt (List.rev stmts) })
