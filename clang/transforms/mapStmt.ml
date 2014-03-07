open Clang.Ast


let mapCompoundStmt map_stmt state stmt stmts =
  let stmts =
    List.fold_left (fun stmts stmt ->
      let (state, stmt) = map_stmt state stmt in
      match state with
      | [] -> stmt :: stmts
      | xs -> xs @ stmts
    ) [] stmts
  in

  (state, { stmt with s = CompoundStmt (List.rev stmts) })
