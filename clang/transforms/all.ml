let (%) f g = fun x -> f (g x)

let transform_decl =
  SplitInitialisers.transform_decl
  % SimplifyDeclStmt.transform_decl
