open Util.Prelude

let transform_decl clang decl =
  decl
  |> ForToWhile.transform_decl clang
  |> SwitchToIf.transform_decl clang
  |> RemoveImplicitCast.transform_decl clang
  |> SimplifyDeclStmt.transform_decl clang
  |> LiftConditionals.transform_decl clang
  |> PostIncrDecr.transform_decl clang
  |> SplitInitialisers.transform_decl clang
  |> PreIncrDecr.transform_decl clang
  (*|> NameAnonymousTypes.transform_decl clang*)
