open Clang.Prelude

let transform_decl clang decl =
  decl
  |> RemoveImplicitCast.transform_decl clang
  |> SimplifyDeclStmt.transform_decl clang
  |> SplitInitialisers.transform_decl clang
  |> LiftConditionals.transform_decl clang
