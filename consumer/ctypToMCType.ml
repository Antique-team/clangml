open Clang
open Ast
open C_sig

module Log = Util.Logger.Make(struct let tag = "CtypToMCType" end)

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


let rec c_type_of_ctyp ctyp = match ctyp.t with
  | BuiltinType bt ->
      c_type_of_builtin_type bt

  | ConstantArrayType (memty, size) ->
      Ctarray (c_type_of_ctyp memty, size)

  | TypedefType name ->
      assert (name <> "");
      Ctnamed { cnt_name = name; cnt_type = Ctvoid; }

  | ElaboratedType ty ->
      c_type_of_ctyp ty

  | EnumType name
  | RecordType (_, name) ->
      (* TODO: this is wrong. *)
      (*assert (name <> "");*)
      if name <> "" then
        Ctnamed { cnt_name = name; cnt_type = Ctvoid; }
      else
        Ctvoid

  | ParenType inner ->
      c_type_of_ctyp inner

  | PointerType { t = FunctionNoProtoType _
                    | FunctionProtoType _
                }
  | FunctionNoProtoType _
  | FunctionProtoType _ ->
      (* MemCAD ignores the type of functions (and function pointers). *)
      Ctvoid

  | PointerType pointee ->
      Ctptr (Some (c_type_of_ctyp pointee))

  | TypeOfExprType _ -> Log.unimp "TypeOfExprType"
  | TypeOfType _ -> Log.unimp "TypeOfType"
  | VariableArrayType _ -> Log.unimp "VariableArrayType"
  | IncompleteArrayType _ -> Log.unimp "IncompleteArrayType"
  | DecayedType _ -> Log.unimp "IncompleteArrayType"

  | ty -> Log.unimp "%a" Show.format<ctyp_> ty


let map_types types =
  Util.DenseIntMap.mapk
    (fun _i ctyp -> c_type_of_ctyp ctyp)
    types
