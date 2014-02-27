open C_sig
open Data_structures
open ClangAst


let c_type_of_builtin_type = function
  | BT_Void -> Ctvoid
  | BT_Char16
  | BT_Char32
  | BT_Short
  | BT_Long
  | BT_LongLong
  | BT_Int128
  | BT_UShort
  | BT_ULong
  | BT_ULongLong
  | BT_UInt128
  | BT_UInt
  | BT_Int -> Ctint
  | BT_Bool
  | BT_SChar
  | BT_UChar
  | BT_Char_S
  | BT_Char_U -> Ctchar
  | _ -> Ctint


let rec c_type_of_type_loc = function
  | BuiltinTypeLoc (_, bt) ->
      c_type_of_builtin_type bt

  | ConstantArrayTypeLoc (_, memty, size) ->
      Ctarray (c_type_of_type_loc memty, size)

  | TypedefTypeLoc (_, name) ->
      Ctnamed { cnt_name = name; cnt_type = Ctvoid; }

  | ElaboratedTypeLoc (_, ty) ->
      c_type_of_type_loc ty

  | EnumTypeLoc (_, name)
  | RecordTypeLoc (_, _, name) ->
      Ctnamed { cnt_name = name; cnt_type = Ctvoid; }

  | PointerTypeLoc (_, pointee) ->
      Ctptr (Some (c_type_of_type_loc pointee))

  | ParenTypeLoc (_, inner) ->
      c_type_of_type_loc inner

  | FunctionProtoTypeLoc (sloc, _, _) as ty ->
      Format.fprintf Format.std_formatter "%a%a\n"
        ClangPp.pp_sloc sloc
        ClangPp.pp_tloc ty;
      failwith "FunctionProtoTypeLoc"

  | TypeOfExprTypeLoc _ -> failwith "TypeOfExprTypeLoc"
  | TypeOfTypeLoc _ -> failwith "TypeOfTypeLoc"
  | QualifiedTypeLoc _ -> failwith "QualifiedTypeLoc"
  | FunctionNoProtoTypeLoc _ -> failwith "FunctionNoProtoTypeLoc"
  | VariableArrayTypeLoc _ -> failwith "VariableArrayTypeLoc"
  | IncompleteArrayTypeLoc _ -> failwith "IncompleteArrayTypeLoc"

  | UnimpTypeLoc (_, name) -> failwith ("Unimplemented: " ^ name)

  | _ -> Ctint


let collect_decls prog = function
  | EmptyDecl _ -> prog
  | FunctionDecl { fd_type; fd_name; fd_body; } ->
      prog

  | TypedefDecl (_, ty, name) ->
      let c_type = c_type_of_type_loc ty in
      { prog with
        cp_types = StringMap.add name c_type prog.cp_types;
      }

  | VarDecl (_, ty, name, init) ->
      prog
  | RecordDecl (_, name, members) ->
      prog
  | EnumDecl (_, name, enumerators) ->
      prog

  | EnumConstantDecl    _ -> failwith "EnumConstantDecl found at file scope"
  | FieldDecl           _ -> failwith "FieldDecl found at file scope"
  | ParmVarDecl         _ -> failwith "ParmVarDecl found at file scope"
  | TranslationUnitDecl _ -> failwith "nested TranslationUnitDecl found"

  | UnimpDecl (_, name) -> failwith ("Unimplemented: " ^ name)


let c_prog_from_decl = function
  | TranslationUnitDecl (_, decls) ->
      List.fold_left collect_decls C_utils.empty_unit decls
  | _ -> failwith "c_prog_from_decl requires a translation unit"
