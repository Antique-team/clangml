open Clang.Prelude

let transform_decl decl =
  decl
  |> SimplifyDeclStmt.transform_decl
  |> SplitInitialisers.transform_decl
  |> LiftConditionals.transform_decl
