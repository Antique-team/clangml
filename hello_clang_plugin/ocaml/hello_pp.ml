(* Hello pretty printer *)

open Hello_ast


let string_of_builtin_type = function
  | BT_Void -> "void"
  | BT_Bool -> "bool"
  | BT_Char_U -> "char_u"
  | BT_UChar -> "unsigned char"
  | BT_WChar_U -> "wchar_u"
  | BT_Char16 -> "char16_t"
  | BT_Char32 -> "char32_t"
  | BT_UShort -> "unsigned short"
  | BT_UInt -> "unsigned int"
  | BT_ULong -> "unsigned long"
  | BT_ULongLong -> "unsigned long long"
  | BT_UInt128 -> "__uint128"
  | BT_Char_S -> "char_s"
  | BT_SChar -> "signed char"
  | BT_WChar_S -> "wchar_s"
  | BT_Short -> "short"
  | BT_Int -> "int"
  | BT_Long -> "long"
  | BT_LongLong -> "long long"
  | BT_Int128 -> "__int128"
  | BT_Half -> "half"
  | BT_Float -> "float"
  | BT_Double -> "double"
  | BT_LongDouble -> "long double"
  | BT_NullPtr -> "nullptr_t"
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


let pp_option f ff = function
  | None -> Format.pp_print_string ff "<null>"
  | Some x -> f ff x


let rec pp_expr ff = function
  | UnimpExpr name ->
      Format.fprintf ff "<%s>" name

  | CharacterLiteral c -> Format.pp_print_string ff (Char.escaped c)
  | IntegerLiteral i -> Format.pp_print_int ff i
  | FloatingLiteral f -> Format.pp_print_float ff f
  | StringLiteral s -> Format.fprintf ff "\"%s\"" (String.escaped s)
  | UnaryOperator (op, e) ->
      if is_prefix op then
        Format.fprintf ff "(%s %a)" (string_of_unary_op op) pp_expr e
      else
        Format.fprintf ff "(%a %s)" pp_expr e (string_of_unary_op op)
  | BinaryOperator (op, e1, e2) ->
      Format.fprintf ff "(%a %s %a)"
        pp_expr e1
        (string_of_binary_op op)
        pp_expr e2

  | DeclRefExpr name ->
      Format.pp_print_string ff name
  | ImplicitCastExpr expr ->
      Format.fprintf ff "%a"
        pp_expr expr
  | ParenExpr expr ->
      Format.fprintf ff "(%a)"
        pp_expr expr


let rec pp_stmt ff = function
  | UnimpStmt name ->
      Format.fprintf ff "<%s>" name

  | NullStmt ->
      Format.pp_print_string ff ";"
  | BreakStmt ->
      Format.pp_print_string ff "break;"
  | ContinueStmt ->
      Format.pp_print_string ff "continue;"
  | ExprStmt e ->
      Format.fprintf ff "%a;"
        pp_expr e
  | CompoundStmt ss ->
      Format.fprintf ff "{ %a }"
        (Formatx.pp_list ~sep:(Formatx.pp_sep "") pp_stmt) ss
  | ReturnStmt None ->
      Format.fprintf ff "return;"
  | ReturnStmt (Some e) ->
      Format.fprintf ff "return %a;"
        pp_expr e
  | CaseStmt (lhs, None, sub) ->
      Format.fprintf ff "case %a: %a"
        pp_expr lhs
        pp_stmt sub
  | CaseStmt (lhs, Some rhs, sub) ->
      Format.fprintf ff "case %a ... %a: %a"
        pp_expr lhs
        pp_expr rhs
        pp_stmt sub
  | ForStmt (init, cond, inc, body) ->
      Format.fprintf ff "for (%a%a;%a) %a"
        (pp_option pp_stmt) init
        (pp_option pp_expr) cond
        (pp_option pp_expr) inc
        pp_stmt body
  | SwitchStmt (cond, body) ->
      Format.fprintf ff "switch (%a) %a"
        pp_expr cond
        pp_stmt body
  | IfStmt (cond, thn, None) ->
      Format.fprintf ff "if (%a) %a"
        pp_expr cond
        pp_stmt thn
  | IfStmt (cond, thn, Some els) ->
      Format.fprintf ff "if (%a) %a else %a"
        pp_expr cond
        pp_stmt thn
        pp_stmt els
  | DeclStmt decls ->
      Format.fprintf ff "%a;"
        (Formatx.pp_list pp_decl) decls


and pp_type ff = function
  | BuiltinTypeLoc bt ->
      Format.fprintf ff "%s"
        (string_of_builtin_type bt)
  | TypedefTypeLoc name ->
      Format.fprintf ff "%s"
        name
  | PointerTypeLoc ty ->
      Format.fprintf ff "%a ptr"
        pp_type ty
  | FunctionNoProtoTypeLoc ty ->
      Format.fprintf ff "? -> %a"
        pp_type ty
  | FunctionProtoTypeLoc (ty, args) ->
      Format.fprintf ff "(%a) -> %a"
        (Formatx.pp_list pp_decl) args
        pp_type ty
  | ConstantArrayTypeLoc (ty, size) ->
      Format.fprintf ff "%a[%d]"
        pp_type ty
        size
  | IncompleteArrayTypeLoc (ty) ->
      Format.fprintf ff "%a[]"
        pp_type ty


and pp_decl ff = function
  | UnimpDecl name ->
      Format.fprintf ff "<%s>" name

  | TranslationUnitDecl dd ->
      Formatx.pp_list ~sep:(Formatx.pp_sep "") pp_decl ff dd
  | TypedefDecl (ty, name) ->
      Format.fprintf ff "typedef %s : %a;"
        name
        pp_type ty
  | FunctionDecl (ty, name, body) ->
      Format.fprintf ff "@[<v2>%a@]@, = %a"
        pp_named_arg (name, ty)
        (pp_option pp_stmt) body
  | VarDecl (ty, name)
  | ParmVarDecl (ty, name) ->
      pp_named_arg ff (name, ty)


and pp_named_arg ff = function
  | ("", ty) ->
      pp_type ff ty
  | (name, ty) ->
      Format.fprintf ff "%s : %a"
        name
        pp_type ty
