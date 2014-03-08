open Clang.Ast


let mapCompoundStmt map_stmt v state stmt stmts =
  let stmts =
    List.fold_left (fun stmts stmt ->
      let (state, stmt) = map_stmt v state stmt in
      match state with
      | [] -> stmt :: stmts
      | replacement -> replacement @ stmts
    ) [] stmts
  in

  (state, { stmt with s = CompoundStmt (List.rev stmts) })
