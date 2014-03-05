(* Clang AST pretty printer *)

open ClangAst

let string_of_qualifier = function
  | TQ_Const -> "const"
  | TQ_Volatile -> "volatile"
  | TQ_Restrict -> "restrict"
  | TQ_Weak -> "weak"
  | TQ_Strong -> "strong"
  | TQ_OCL_ExplicitNone -> "no_lifetime"
  | TQ_OCL_Strong -> "strong_lifetime"
  | TQ_OCL_Weak -> "weak_lifetime"
  | TQ_OCL_Autoreleasing -> "autoreleasing"

let string_of_predefined_ident = function
  | PI_Func -> "__func__"
  | PI_Function -> "__FUNCTION__"
  | PI_LFunction -> "__LFUNCTION__"
  | PI_FuncDName -> "__FUNCDNAME__"
  | PI_PrettyFunction -> "__PRETTY_FUNCTION__"
  | PI_PrettyFunctionNoVirtual -> "__PRETTY_FUNCTION_NO_VIRTUAL__"

let string_of_tag_type_kind = function
  | TTK_Struct -> "struct"
  | TTK_Interface -> "__interface"
  | TTK_Union -> "union"
  | TTK_Class -> "class"
  | TTK_Enum -> "enum"

let string_of_elaborated_type_keyword = function
  | ETK_Struct -> "struct"
  | ETK_Interface -> "__interface"
  | ETK_Union -> "union"
  | ETK_Class -> "class"
  | ETK_Enum -> "enum"
  | ETK_Typename -> "typename"
  | ETK_None -> ""

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

let pp_sloc ff sloc =
  if ClangSloc.is_valid sloc then
    Format.fprintf ff "# %d \"%s\"\n"
      sloc.loc_s_line
      sloc.loc_s_filename
  else
    Format.pp_print_string ff "# <invalid sloc>\n"


let rec pp_designator ff = function
  | { dr = FieldDesignator (name) } ->
      Format.fprintf ff ".%s" name
  | { dr = ArrayDesignator (index) } ->
      Format.fprintf ff "[%a]"
        pp_expr index
  | { dr = ArrayRangeDesignator (left, right) } ->
      Format.fprintf ff "[%a ... %a]"
        pp_expr left
        pp_expr right


and pp_expr ff = function
  | { e = UnimpExpr (name) } ->
      Format.fprintf ff "<%s>" name

  | { e = CharacterLiteral (c) } -> Format.pp_print_string ff (Char.escaped c)
  | { e = IntegerLiteral (i) } -> Format.pp_print_int ff i
  | { e = FloatingLiteral (f) } -> Format.pp_print_float ff f
  | { e = StringLiteral (s) } -> Format.fprintf ff "\"%s\"" (String.escaped s)
  | { e = UnaryOperator (op, e) } ->
      if is_prefix op then
        Format.fprintf ff "(%s %a)" (string_of_unary_op op) pp_expr e
      else
        Format.fprintf ff "(%a %s)" pp_expr e (string_of_unary_op op)
  | { e = BinaryOperator (op, e1, e2) } ->
      Format.fprintf ff "(%a %s %a)"
        pp_expr e1
        (string_of_binary_op op)
        pp_expr e2

  | { e = DeclRefExpr (name) } ->
      Format.pp_print_string ff name
  | { e = PredefinedExpr (kind) } ->
      Format.pp_print_string ff (string_of_predefined_ident kind)
  | { e = ImplicitCastExpr (expr) } ->
      Format.fprintf ff "%a"
        pp_expr expr
  | { e = CompoundLiteralExpr (ty, expr) }
  | { e = CStyleCastExpr (ty, expr) } ->
      Format.fprintf ff "(%a)%a"
        pp_tloc ty
        pp_expr expr
  | { e = ParenExpr (expr) } ->
      Format.fprintf ff "(%a)"
        pp_expr expr
  | { e = VAArgExpr (sub, ty) } ->
      Format.fprintf ff "va_arg (%a, %a)"
        pp_expr sub
        pp_tloc ty
  | { e = CallExpr (callee, args) } ->
      Format.fprintf ff "%a (%a)"
        pp_expr callee
        (Formatx.pp_list pp_expr) args
  | { e = MemberExpr (base, member, is_arrow) } ->
      Format.fprintf ff "%a%s%s"
        pp_expr base
        (if is_arrow then "->" else ".")
        member
  | { e = ConditionalOperator (cond, true_expr, false_expr) } ->
      Format.fprintf ff "%a ? %a : %a"
        pp_expr cond
        pp_expr true_expr
        pp_expr false_expr
  | { e = DesignatedInitExpr (designators, init) } ->
      Format.fprintf ff "%a = %a"
        (Formatx.pp_list ~sep:(Formatx.pp_sep "") pp_designator) designators
        pp_expr init
  | { e = InitListExpr (inits) } ->
      Format.fprintf ff "{ %a }"
        (Formatx.pp_list pp_expr) inits
  | { e = ImplicitValueInitExpr } ->
      Format.pp_print_string ff "<implicit value>"
  | { e = ArraySubscriptExpr (base, idx) } ->
      Format.fprintf ff "%a[%a]"
        pp_expr base
        pp_expr idx
  | { e = StmtExpr (stmt) } ->
      Format.fprintf ff "(%a)"
        pp_stmt stmt
  | { e = SizeOfExpr (expr) } ->
      Format.fprintf ff "sizeof %a"
        pp_expr expr
  | { e = SizeOfType (ty) } ->
      Format.fprintf ff "sizeof (%a)"
        pp_tloc ty
  | { e = AlignOfExpr (expr) } ->
      Format.fprintf ff "alignof %a"
        pp_expr expr
  | { e = AlignOfType (ty) } ->
      Format.fprintf ff "alignof (%a)"
        pp_tloc ty
  | { e = VecStepExpr (expr) } ->
      Format.fprintf ff "vec_step %a"
        pp_expr expr
  | { e = VecStepType (ty) } ->
      Format.fprintf ff "vec_step (%a)"
        pp_tloc ty


and pp_stmt ff = function
  | { s = UnimpStmt (name) } ->
      Format.fprintf ff "<%s>" name

  | { s = NullStmt } ->
      Format.pp_print_string ff ";"
  | { s = BreakStmt } ->
      Format.pp_print_string ff "break;"
  | { s = ContinueStmt } ->
      Format.pp_print_string ff "continue;"
  | { s = LabelStmt (name, sub) } ->
      Format.fprintf ff "%s: %a"
        name
        pp_stmt sub
  | { s = GotoStmt name } ->
      Format.fprintf ff "goto %s;"
        name
  | { s = ExprStmt e } ->
      Format.fprintf ff "%a;"
        pp_expr e
  | { s = CompoundStmt (ss) } ->
      Format.fprintf ff "{ %a }"
        (Formatx.pp_list ~sep:(Formatx.pp_sep "") pp_stmt) ss
  | { s = ReturnStmt (None) } ->
      Format.fprintf ff "return;"
  | { s = ReturnStmt (Some e) } ->
      Format.fprintf ff "return %a;"
        pp_expr e
  | { s = DefaultStmt (sub) } ->
      Format.fprintf ff "default: %a"
        pp_stmt sub
  | { s = CaseStmt (lhs, None, sub) } ->
      Format.fprintf ff "case %a: %a"
        pp_expr lhs
        pp_stmt sub
  | { s = CaseStmt (lhs, Some rhs, sub) } ->
      Format.fprintf ff "case %a ... %a: %a"
        pp_expr lhs
        pp_expr rhs
        pp_stmt sub
  | { s = ForStmt (init, cond, inc, body) } ->
      Format.fprintf ff "for (%a%a;%a) %a"
        (pp_option pp_stmt) init
        (pp_option pp_expr) cond
        (pp_option pp_expr) inc
        pp_stmt body
  | { s = WhileStmt (cond, body) } ->
      Format.fprintf ff "while (%a) %a"
        pp_expr cond
        pp_stmt body
  | { s = DoStmt (body, cond) } ->
      Format.fprintf ff "do %a while (%a)"
        pp_stmt body
        pp_expr cond
  | { s = SwitchStmt (cond, body) } ->
      Format.fprintf ff "switch (%a) %a"
        pp_expr cond
        pp_stmt body
  | { s = IfStmt (cond, thn, None) } ->
      Format.fprintf ff "if (%a) %a"
        pp_expr cond
        pp_stmt thn
  | { s = IfStmt (cond, thn, Some els) } ->
      Format.fprintf ff "if (%a) %a else %a"
        pp_expr cond
        pp_stmt thn
        pp_stmt els
  | { s = DeclStmt (decls) } ->
      Format.fprintf ff "%a;"
        (Formatx.pp_list pp_decl) decls


and pp_tloc ff = function
  | { tl = UnimpTypeLoc (name) } ->
      Format.fprintf ff "<%s>" name

  | { tl = QualifiedTypeLoc (unqual, quals, addr_space) } ->
      Format.fprintf ff "%a %a%s"
        pp_tloc unqual
        Formatx.(pp_list ~sep:(pp_sep "") pp_print_string)
          (List.map string_of_qualifier quals)
        (match addr_space with
         | None -> ""
         | Some aspace -> " addr_space_" ^ string_of_int aspace)
  | { tl = BuiltinTypeLoc (bt) } ->
      Format.fprintf ff "%s"
        (string_of_builtin_type bt)
  | { tl = TypedefTypeLoc (name) } ->
      Format.fprintf ff "%s"
        name
  | { tl = TypeOfExprTypeLoc (expr) } ->
      Format.fprintf ff "typeof (%a)"
        pp_expr expr
  | { tl = TypeOfTypeLoc (ty) } ->
      Format.fprintf ff "typeof (%a)"
        pp_tloc ty
  | { tl = ParenTypeLoc (ty) } ->
      Format.fprintf ff "(%a)"
        pp_tloc ty
  | { tl = PointerTypeLoc (ty) } ->
      Format.fprintf ff "%a ptr"
        pp_tloc ty
  | { tl = FunctionNoProtoTypeLoc (ty) } ->
      Format.fprintf ff "? -> %a"
        pp_tloc ty
  | { tl = FunctionProtoTypeLoc (ty, args) } ->
      Format.fprintf ff "(%a) -> %a"
        (Formatx.pp_list pp_decl) args
        pp_tloc ty
  | { tl = ConstantArrayTypeLoc (ty, size) } ->
      Format.fprintf ff "%a[%d]"
        pp_tloc ty
        size
  | { tl = VariableArrayTypeLoc (ty, size) } ->
      Format.fprintf ff "%a[%a]"
        pp_tloc ty
        pp_expr size
  | { tl = IncompleteArrayTypeLoc (ty) } ->
      Format.fprintf ff "%a[]"
        pp_tloc ty
  | { tl = ElaboratedTypeLoc (ty) } ->
      Format.fprintf ff "%a"
        pp_tloc ty
  | { tl = RecordTypeLoc (kind, name) } ->
      Format.fprintf ff "%s %s"
        (string_of_tag_type_kind kind)
        (if name = "" then "<anonymous>" else name)
  | { tl = EnumTypeLoc (name) } ->
      Format.pp_print_string ff
        (if name = "" then "<anonymous>" else name)


and pp_type_ ff = function
  | UnimpType name ->
      Format.fprintf ff "<%s>" name

  | BuiltinType bt ->
      Format.fprintf ff "%s"
        (string_of_builtin_type bt)
  | TypedefType name ->
      Format.fprintf ff "%s"
        name
  | TypeOfExprType expr ->
      Format.fprintf ff "typeof (%a)"
        pp_expr expr
  | TypeOfType ty ->
      Format.fprintf ff "typeof (%a)"
        pp_type ty
  | ParenType ty ->
      Format.fprintf ff "(%a)"
        pp_type ty
  | PointerType ty ->
      Format.fprintf ff "%a ptr"
        pp_type ty
  | FunctionNoProtoType ty ->
      Format.fprintf ff "? -> %a"
        pp_type ty
  | FunctionProtoType (ty, args) ->
      Format.fprintf ff "(%a) -> %a"
        (Formatx.pp_list pp_type) args
        pp_type ty
  | ConstantArrayType (ty, size) ->
      Format.fprintf ff "%a[%d]"
        pp_type ty
        size
  | VariableArrayType (ty, size) ->
      Format.fprintf ff "%a[%a]"
        pp_type ty
        pp_expr size
  | IncompleteArrayType (ty) ->
      Format.fprintf ff "%a[]"
        pp_type ty
  | ElaboratedType (ty) ->
      Format.fprintf ff "%a"
        pp_type ty
  | RecordType (kind, name) ->
      Format.fprintf ff "%s %s"
        (string_of_tag_type_kind kind)
        (if name = "" then "<anonymous>" else name)
  | EnumType (name) ->
      Format.pp_print_string ff
        (if name = "" then "<anonymous>" else name)
  | DecayedType (decayed, original) ->
      Format.fprintf ff "%a"
        pp_type decayed


and pp_type ff = function
  | { t = unqual; t_qual = quals; t_aspace = addr_space; } ->
      Format.fprintf ff "%a %a%s"
        pp_type_ unqual
        Formatx.(pp_list ~sep:(pp_sep "") pp_print_string)
          (List.map string_of_qualifier quals)
        (match addr_space with
         | None -> ""
         | Some aspace -> " addr_space_" ^ string_of_int aspace)


and pp_decl ff = function
  | { d = UnimpDecl name } ->
      Format.fprintf ff "<%s>" name

  | { d = EmptyDecl } ->
      Format.pp_print_string ff ";"
  | { d = TranslationUnitDecl decls } ->
      Formatx.pp_list ~sep:(Formatx.pp_sep "") pp_decl ff decls
  | { d = TypedefDecl (ty, name) } ->
      Format.fprintf ff "typedef %s : %a;"
        name
        pp_tloc ty
  | { d = FunctionDecl (fd_type, fd_name, fd_body) } ->
      Format.fprintf ff "@[<v2>%a@]@, = %a"
        pp_named_arg (fd_name, fd_type)
        (pp_option pp_stmt) fd_body
  | { d = VarDecl (ty, name, Some init) } ->
      Format.fprintf ff "%a = %a"
        pp_named_arg (name, ty)
        pp_expr init
  | { d = VarDecl (ty, name, None) }
  | { d = ParmVarDecl (ty, name) } ->
      pp_named_arg ff (name, ty)
  | { d = RecordDecl (name, members) } ->
      Format.fprintf ff "struct %s { %a };"
        name
        (Formatx.pp_list ~sep:(Formatx.pp_sep "") pp_decl) members
  | { d = FieldDecl (ty, name, bitwidth, init) } ->
      Format.fprintf ff "%a;"
        pp_named_arg (name, ty)
  | { d = EnumDecl (name, enumerators) } ->
      Format.fprintf ff "enum %s { %a };"
        name
        (Formatx.pp_list ~sep:(Formatx.pp_sep "") pp_decl) enumerators
  | { d = EnumConstantDecl (name, None) } ->
      Format.fprintf ff "%s;"
        name
  | { d = EnumConstantDecl (name, Some init) } ->
      Format.fprintf ff "%s = %a;"
        name
        pp_expr init


and pp_named_arg ff = function
  | ("", ty) ->
      pp_tloc ff ty
  | (name, ty) ->
      Format.fprintf ff "%s : %a"
        name
        pp_tloc ty
