val c_type_of_ctyp : Clang.Ast.ctyp -> C_sig.c_type

val map_types
  :  (Clang.Ast.ctyp, Clang.Ast.ctyp) Util.DenseIntMap.t
  -> (Clang.Ast.ctyp, C_sig.c_type  ) Util.DenseIntMap.t
