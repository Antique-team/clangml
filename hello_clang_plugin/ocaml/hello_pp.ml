(* Hello pretty printer *)

open Hello_ast


let string_of_builtin_type = function
  | BT_Void -> "void"
  | BT_Bool -> "bool"
  | BT_Char_U -> "char_u"
  | BT_UChar -> "uchar"
  | BT_WChar_U -> "wchar_u"
  | BT_Char16 -> "char16"
  | BT_Char32 -> "char32"
  | BT_UShort -> "ushort"
  | BT_UInt -> "uint"
  | BT_ULong -> "ulong"
  | BT_ULongLong -> "ulonglong"
  | BT_UInt128 -> "uint128"
  | BT_Char_S -> "char_s"
  | BT_SChar -> "schar"
  | BT_WChar_S -> "wchar_s"
  | BT_Short -> "short"
  | BT_Int -> "int"
  | BT_Long -> "long"
  | BT_LongLong -> "longlong"
  | BT_Int128 -> "int128"
  | BT_Half -> "half"
  | BT_Float -> "float"
  | BT_Double -> "double"
  | BT_LongDouble -> "longdouble"
  | BT_NullPtr -> "nullptr"
  | BT_ObjCId -> "objcid"
  | BT_ObjCClass -> "objcclass"
  | BT_ObjCSel -> "objcsel"
  | BT_OCLImage1d -> "oclimage1d"
  | BT_OCLImage1dArray -> "oclimage1darray"
  | BT_OCLImage1dBuffer -> "oclimage1dbuffer"
  | BT_OCLImage2d -> "oclimage2d"
  | BT_OCLImage2dArray -> "oclimage2darray"
  | BT_OCLImage3d -> "oclimage3d"
  | BT_OCLSampler -> "oclsampler"
  | BT_OCLEvent -> "oclevent"
  | BT_Dependent -> "dependent"
  | BT_Overload -> "overload"
  | BT_BoundMember -> "boundmember"
  | BT_PseudoObject -> "pseudoobject"
  | BT_UnknownAny -> "unknownany"
  | BT_BuiltinFn -> "builtinfn"
  | BT_ARCUnbridgedCast -> "arcunbridgedcast"


let is_prefix = function
  | UO_PostInc
  | UO_PostDec -> false
  | _ -> true


let string_of_unary_op = function
  | UO_PostInc		-> "++"
  | UO_PostDec		-> "--"
  | UO_PreInc		-> "++"
  | UO_PreDec		-> "--"
  | UO_AddrOf		-> "&"
  | UO_Deref		-> "*"
  | UO_Plus		-> "+"
  | UO_Minus		-> "-"
  | UO_Not		-> "~"
  | UO_LNot		-> "!"
  | UO_Real		-> "__real"
  | UO_Imag		-> "__imag"
  | UO_Extension	-> "__extension__"


let string_of_binary_op = function
  | BO_PtrMemD		-> "->*"
  | BO_PtrMemI		-> "->*"
  | BO_Mul		-> "*"
  | BO_Div		-> "/"
  | BO_Rem		-> "%"
  | BO_Add		-> "+"
  | BO_Sub		-> "-"
  | BO_Shl		-> "<<"
  | BO_Shr		-> ">>"
  | BO_LT		-> "<"
  | BO_GT		-> ">"
  | BO_LE		-> "<="
  | BO_GE		-> ">="
  | BO_EQ		-> "=="
  | BO_NE		-> "!="
  | BO_And		-> "&"
  | BO_Xor		-> "^"
  | BO_Or		-> "|"
  | BO_LAnd		-> "&&"
  | BO_LOr		-> "||"
  | BO_Assign		-> "="
  | BO_Comma		-> ","

  | BO_MulAssign	-> "*="
  | BO_DivAssign	-> "/="
  | BO_RemAssign	-> "%="
  | BO_AddAssign	-> "+="
  | BO_SubAssign	-> "-="
  | BO_ShlAssign	-> "<<="
  | BO_ShrAssign	-> ">>="
  | BO_AndAssign	-> "&="
  | BO_OrAssign		-> "|="
  | BO_XorAssign	-> "^*"


let rec pp_expr ff = function
  | Unit -> Format.pp_print_string ff "Unit"

  | CharacterLiteral c -> Format.pp_print_string ff (Char.escaped c)
  | IntegerLiteral i -> Format.pp_print_int ff i
  | FloatingLiteral f -> Format.pp_print_float ff f
  | StringLiteral s -> Format.fprintf ff "\"%s\"" (String.escaped s)
  | UnaryOperator (op, e) ->
      if is_prefix op then
        Format.fprintf ff "(%s %a)" (string_of_unary_op op) pp_expr e
      else
        Format.fprintf ff "(%a %s)" pp_expr e (string_of_unary_op op)
  | BinaryOperator (op, e1, e2) -> Format.fprintf ff "(%a %s %a)"
                                     pp_expr e1
                                     (string_of_binary_op op)
                                     pp_expr e2


let rec pp_stmt ff = function
  | Skip -> Format.pp_print_string ff "Skip"
  | Print e -> Format.fprintf ff "Print %a" pp_expr e
  | Block ss -> Formatx.pp_list ~sep:(Formatx.pp_sep "") pp_stmt ff ss

  | CompoundStmt ss ->
      Format.fprintf ff "{ %a }"
        (Formatx.pp_list ~sep:(Formatx.pp_sep "") pp_stmt) ss
  | ReturnStmt e -> Format.fprintf ff "return %a;" pp_expr e


let rec pp_type ff = function
  | BuiltinTypeLoc bt ->
      Format.fprintf ff "%s"
        (string_of_builtin_type bt)
  | TypedefTypeLoc ->
      Format.fprintf ff "tdefty"
  | FunctionNoProtoTypeLoc ty ->
      Format.fprintf ff "%a"
        pp_type ty
  | ConstantArrayTypeLoc ty ->
      Format.fprintf ff "%a[]"
        pp_type ty


let rec pp_decl ff = function
  | TranslationUnitDecl dd ->
      Formatx.pp_list ~sep:(Formatx.pp_sep "") pp_decl ff dd
  | TypedefDecl ty ->
      Format.fprintf ff "typedef %a;"
        pp_type ty
  | FunctionDecl (ty, name, body) ->
      Format.fprintf ff "%s : %a %a"
        name
        pp_type ty
        pp_stmt body
