val map_types
  : Clang.Api.clang
  -> (Clang.Ast.ctyp, Clang.Ast.ctyp) Util.DenseIntMap.t
  -> (Clang.Ast.ctyp, C_sig.c_type  ) Util.DenseIntMap.t
