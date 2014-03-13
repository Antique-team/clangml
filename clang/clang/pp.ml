(* Clang AST pretty printer *)

open Ast
open Util

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

let string_of_predefined_expr = function
  | PE_Func -> "__func__"
  | PE_Function -> "__FUNCTION__"
  | PE_LFunction -> "__LFUNCTION__"
  | PE_FuncDName -> "__FUNCDNAME__"
  | PE_PrettyFunction -> "__PRETTY_FUNCTION__"
  | PE_PrettyFunctionNoVirtual -> "__PRETTY_FUNCTION_NO_VIRTUAL__"

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

let pp_sloc ff ploc =
  if Sloc.is_valid_presumed ploc then
    Format.fprintf ff "# %d \"%s\"\n"
      ploc.Sloc.loc_line
      ploc.Sloc.loc_filename
  else
    Format.pp_print_string ff "# <invalid sloc>\n"


let rec pp_desg_ ff = function
  | FieldDesignator name ->
      Format.fprintf ff ".%s" name
  | ArrayDesignator index ->
      Format.fprintf ff "[%a]"
        pp_expr index
  | ArrayRangeDesignator (left, right) ->
      Format.fprintf ff "[%a ... %a]"
        pp_expr left
        pp_expr right

and pp_desg ff desg =
  pp_desg_ ff desg.dr


and pp_expr_ ff = function
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
  | PredefinedExpr kind ->
      Format.pp_print_string ff (string_of_predefined_expr kind)
  | ImplicitCastExpr (_, expr) ->
      Format.fprintf ff "%a"
        pp_expr expr
  | CompoundLiteralExpr (ty, expr)
  | CStyleCastExpr (_, ty, expr) ->
      Format.fprintf ff "(%a)%a"
        pp_tloc ty
        pp_expr expr
  | ParenExpr expr ->
      Format.fprintf ff "(%a)"
        pp_expr expr
  | VAArgExpr (sub, ty) ->
      Format.fprintf ff "va_arg (%a, %a)"
        pp_expr sub
        pp_tloc ty
  | CallExpr (callee, args) ->
      Format.fprintf ff "%a (%a)"
        pp_expr callee
        (Formatx.pp_list pp_expr) args
  | MemberExpr (base, member, is_arrow) ->
      Format.fprintf ff "%a%s%s"
        pp_expr base
        (if is_arrow then "->" else ".")
        member
  | ConditionalOperator (cond, true_expr, false_expr) ->
      Format.fprintf ff "%a ? %a : %a"
        pp_expr cond
        pp_expr true_expr
        pp_expr false_expr
  | DesignatedInitExpr (designators, init) ->
      Format.fprintf ff "%a = %a"
        (Formatx.pp_list ~sep:(Formatx.pp_sep "") pp_desg) designators
        pp_expr init
  | InitListExpr inits ->
      Format.fprintf ff "{ %a }"
        (Formatx.pp_list pp_expr) inits
  | ImplicitValueInitExpr ->
      Format.pp_print_string ff "<implicit value>"
  | ArraySubscriptExpr (base, idx) ->
      Format.fprintf ff "%a[%a]"
        pp_expr base
        pp_expr idx
  | StmtExpr stmt ->
      Format.fprintf ff "(%a)"
        pp_stmt stmt
  | AddrLabelExpr label ->
      Format.fprintf ff "&&%s"
        label

  | SizeOfExpr expr ->
      Format.fprintf ff "sizeof %a"
        pp_expr expr
  | SizeOfType ty ->
      Format.fprintf ff "sizeof (%a)"
        pp_tloc ty
  | AlignOfExpr expr ->
      Format.fprintf ff "alignof %a"
        pp_expr expr
  | AlignOfType ty ->
      Format.fprintf ff "alignof (%a)"
        pp_tloc ty
  | VecStepExpr expr ->
      Format.fprintf ff "vec_step %a"
        pp_expr expr
  | VecStepType ty ->
      Format.fprintf ff "vec_step (%a)"
        pp_tloc ty

  | ArrayTypeTraitExpr -> Format.pp_print_string ff "<ArrayTypeTraitExpr>"
  | AsTypeExpr -> Format.pp_print_string ff "<AsTypeExpr>"
  | AtomicExpr -> Format.pp_print_string ff "<AtomicExpr>"
  | BinaryConditionalOperator -> Format.pp_print_string ff "<BinaryConditionalOperator>"
  | BinaryTypeTraitExpr -> Format.pp_print_string ff "<BinaryTypeTraitExpr>"
  | BlockExpr -> Format.pp_print_string ff "<BlockExpr>"
  | ChooseExpr -> Format.pp_print_string ff "<ChooseExpr>"
  | CompoundAssignOperator -> Format.pp_print_string ff "<CompoundAssignOperator>"
  | CUDAKernelCallExpr -> Format.pp_print_string ff "<CUDAKernelCallExpr>"
  | CXXBindTemporaryExpr -> Format.pp_print_string ff "<CXXBindTemporaryExpr>"
  | CXXBoolLiteralExpr -> Format.pp_print_string ff "<CXXBoolLiteralExpr>"
  | CXXConstCastExpr -> Format.pp_print_string ff "<CXXConstCastExpr>"
  | CXXConstructExpr -> Format.pp_print_string ff "<CXXConstructExpr>"
  | CXXDefaultArgExpr -> Format.pp_print_string ff "<CXXDefaultArgExpr>"
  | CXXDefaultInitExpr -> Format.pp_print_string ff "<CXXDefaultInitExpr>"
  | CXXDeleteExpr -> Format.pp_print_string ff "<CXXDeleteExpr>"
  | CXXDependentScopeMemberExpr -> Format.pp_print_string ff "<CXXDependentScopeMemberExpr>"
  | CXXDynamicCastExpr -> Format.pp_print_string ff "<CXXDynamicCastExpr>"
  | CXXFunctionalCastExpr -> Format.pp_print_string ff "<CXXFunctionalCastExpr>"
  | CXXMemberCallExpr -> Format.pp_print_string ff "<CXXMemberCallExpr>"
  | CXXNewExpr -> Format.pp_print_string ff "<CXXNewExpr>"
  | CXXNoexceptExpr -> Format.pp_print_string ff "<CXXNoexceptExpr>"
  | CXXNullPtrLiteralExpr -> Format.pp_print_string ff "<CXXNullPtrLiteralExpr>"
  | CXXOperatorCallExpr -> Format.pp_print_string ff "<CXXOperatorCallExpr>"
  | CXXPseudoDestructorExpr -> Format.pp_print_string ff "<CXXPseudoDestructorExpr>"
  | CXXReinterpretCastExpr -> Format.pp_print_string ff "<CXXReinterpretCastExpr>"
  | CXXScalarValueInitExpr -> Format.pp_print_string ff "<CXXScalarValueInitExpr>"
  | CXXStaticCastExpr -> Format.pp_print_string ff "<CXXStaticCastExpr>"
  | CXXStdInitializerListExpr -> Format.pp_print_string ff "<CXXStdInitializerListExpr>"
  | CXXTemporaryObjectExpr -> Format.pp_print_string ff "<CXXTemporaryObjectExpr>"
  | CXXThisExpr -> Format.pp_print_string ff "<CXXThisExpr>"
  | CXXThrowExpr -> Format.pp_print_string ff "<CXXThrowExpr>"
  | CXXTypeidExpr -> Format.pp_print_string ff "<CXXTypeidExpr>"
  | CXXUnresolvedConstructExpr -> Format.pp_print_string ff "<CXXUnresolvedConstructExpr>"
  | CXXUuidofExpr -> Format.pp_print_string ff "<CXXUuidofExpr>"
  | DependentScopeDeclRefExpr -> Format.pp_print_string ff "<DependentScopeDeclRefExpr>"
  | ExpressionTraitExpr -> Format.pp_print_string ff "<ExpressionTraitExpr>"
  | ExprWithCleanups -> Format.pp_print_string ff "<ExprWithCleanups>"
  | ExtVectorElementExpr -> Format.pp_print_string ff "<ExtVectorElementExpr>"
  | FunctionParmPackExpr -> Format.pp_print_string ff "<FunctionParmPackExpr>"
  | GenericSelectionExpr -> Format.pp_print_string ff "<GenericSelectionExpr>"
  | GNUNullExpr -> Format.pp_print_string ff "<GNUNullExpr>"
  | ImaginaryLiteral -> Format.pp_print_string ff "<ImaginaryLiteral>"
  | LambdaExpr -> Format.pp_print_string ff "<LambdaExpr>"
  | MaterializeTemporaryExpr -> Format.pp_print_string ff "<MaterializeTemporaryExpr>"
  | MSPropertyRefExpr -> Format.pp_print_string ff "<MSPropertyRefExpr>"
  | ObjCArrayLiteral -> Format.pp_print_string ff "<ObjCArrayLiteral>"
  | ObjCBoolLiteralExpr -> Format.pp_print_string ff "<ObjCBoolLiteralExpr>"
  | ObjCBoxedExpr -> Format.pp_print_string ff "<ObjCBoxedExpr>"
  | ObjCBridgedCastExpr -> Format.pp_print_string ff "<ObjCBridgedCastExpr>"
  | ObjCDictionaryLiteral -> Format.pp_print_string ff "<ObjCDictionaryLiteral>"
  | ObjCEncodeExpr -> Format.pp_print_string ff "<ObjCEncodeExpr>"
  | ObjCIndirectCopyRestoreExpr -> Format.pp_print_string ff "<ObjCIndirectCopyRestoreExpr>"
  | ObjCIsaExpr -> Format.pp_print_string ff "<ObjCIsaExpr>"
  | ObjCIvarRefExpr -> Format.pp_print_string ff "<ObjCIvarRefExpr>"
  | ObjCMessageExpr -> Format.pp_print_string ff "<ObjCMessageExpr>"
  | ObjCPropertyRefExpr -> Format.pp_print_string ff "<ObjCPropertyRefExpr>"
  | ObjCProtocolExpr -> Format.pp_print_string ff "<ObjCProtocolExpr>"
  | ObjCSelectorExpr -> Format.pp_print_string ff "<ObjCSelectorExpr>"
  | ObjCStringLiteral -> Format.pp_print_string ff "<ObjCStringLiteral>"
  | ObjCSubscriptRefExpr -> Format.pp_print_string ff "<ObjCSubscriptRefExpr>"
  | OffsetOfExpr -> Format.pp_print_string ff "<OffsetOfExpr>"
  | OpaqueValueExpr -> Format.pp_print_string ff "<OpaqueValueExpr>"
  | PackExpansionExpr -> Format.pp_print_string ff "<PackExpansionExpr>"
  | ParenListExpr -> Format.pp_print_string ff "<ParenListExpr>"
  | PseudoObjectExpr -> Format.pp_print_string ff "<PseudoObjectExpr>"
  | ShuffleVectorExpr -> Format.pp_print_string ff "<ShuffleVectorExpr>"
  | SizeOfPackExpr -> Format.pp_print_string ff "<SizeOfPackExpr>"
  | SubstNonTypeTemplateParmExpr -> Format.pp_print_string ff "<SubstNonTypeTemplateParmExpr>"
  | SubstNonTypeTemplateParmPackExpr -> Format.pp_print_string ff "<SubstNonTypeTemplateParmPackExpr>"
  | TypeTraitExpr -> Format.pp_print_string ff "<TypeTraitExpr>"
  | UnaryTypeTraitExpr -> Format.pp_print_string ff "<UnaryTypeTraitExpr>"
  | UnresolvedLookupExpr -> Format.pp_print_string ff "<UnresolvedLookupExpr>"
  | UnresolvedMemberExpr -> Format.pp_print_string ff "<UnresolvedMemberExpr>"
  | UserDefinedLiteral -> Format.pp_print_string ff "<UserDefinedLiteral>"

and pp_expr ff expr =
  pp_expr_ ff expr.e


and pp_stmt_ ff = function
  | NullStmt ->
      Format.pp_print_string ff ";"
  | BreakStmt ->
      Format.pp_print_string ff "break;"
  | ContinueStmt ->
      Format.pp_print_string ff "continue;"
  | LabelStmt (name, sub) ->
      Format.fprintf ff "%s: %a"
        name
        pp_stmt sub
  | GotoStmt name ->
      Format.fprintf ff "goto %s;"
        name
  | ExprStmt e ->
      Format.fprintf ff "%a;"
        pp_expr e
  | CompoundStmt ss ->
      Format.fprintf ff "@\n@[<v2>{@\n%a@]@\n}"
        (Formatx.pp_list ~sep:(Formatx.pp_sep "") pp_stmt) ss
  | ReturnStmt None ->
      Format.fprintf ff "return;"
  | ReturnStmt (Some e) ->
      Format.fprintf ff "return %a;"
        pp_expr e
  | DefaultStmt sub ->
      Format.fprintf ff "default: %a"
        pp_stmt sub
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
  | WhileStmt (cond, body) ->
      Format.fprintf ff "while (%a) %a"
        pp_expr cond
        pp_stmt body
  | DoStmt (body, cond) ->
      Format.fprintf ff "do %a while (%a)"
        pp_stmt body
        pp_expr cond
  | SwitchStmt (cond, body) ->
      Format.fprintf ff "switch (%a) %a"
        pp_expr cond
        pp_stmt body
  | IfStmt (cond, thn, None) ->
      Format.fprintf ff "if (%a) %a"
        pp_expr cond
        pp_stmt thn
  | IfStmt (cond, thn, Some els) ->
      Format.fprintf ff "if (%a) %a@\nelse %a"
        pp_expr cond
        pp_stmt thn
        pp_stmt els
  | DeclStmt decls ->
      Format.fprintf ff "%a;"
        (Formatx.pp_list pp_decl) decls

  | AttributedStmt -> Format.pp_print_string ff "<AttributedStmt>"
  | CapturedStmt -> Format.pp_print_string ff "<CapturedStmt>"
  | CXXCatchStmt -> Format.pp_print_string ff "<CXXCatchStmt>"
  | CXXForRangeStmt -> Format.pp_print_string ff "<CXXForRangeStmt>"
  | CXXTryStmt -> Format.pp_print_string ff "<CXXTryStmt>"
  | GCCAsmStmt -> Format.pp_print_string ff "<GCCAsmStmt>"
  | IndirectGotoStmt -> Format.pp_print_string ff "<IndirectGotoStmt>"
  | MSAsmStmt -> Format.pp_print_string ff "<MSAsmStmt>"
  | MSDependentExistsStmt -> Format.pp_print_string ff "<MSDependentExistsStmt>"
  | ObjCAtCatchStmt -> Format.pp_print_string ff "<ObjCAtCatchStmt>"
  | ObjCAtFinallyStmt -> Format.pp_print_string ff "<ObjCAtFinallyStmt>"
  | ObjCAtSynchronizedStmt -> Format.pp_print_string ff "<ObjCAtSynchronizedStmt>"
  | ObjCAtThrowStmt -> Format.pp_print_string ff "<ObjCAtThrowStmt>"
  | ObjCAtTryStmt -> Format.pp_print_string ff "<ObjCAtTryStmt>"
  | ObjCAutoreleasePoolStmt -> Format.pp_print_string ff "<ObjCAutoreleasePoolStmt>"
  | ObjCForCollectionStmt -> Format.pp_print_string ff "<ObjCForCollectionStmt>"
  | SEHExceptStmt -> Format.pp_print_string ff "<SEHExceptStmt>"
  | SEHFinallyStmt -> Format.pp_print_string ff "<SEHFinallyStmt>"
  | SEHTryStmt -> Format.pp_print_string ff "<SEHTryStmt>"

and pp_stmt ff stmt =
  pp_stmt_ ff stmt.s


and pp_tloc_ ff = function
  | QualifiedTypeLoc (unqual, quals, addr_space) ->
      Format.fprintf ff "%a %a%s"
        pp_tloc unqual
        Formatx.(pp_list ~sep:(pp_sep "") pp_print_string)
          (List.map string_of_qualifier quals)
        (match addr_space with
         | None -> ""
         | Some aspace -> " addr_space_" ^ string_of_int aspace)
  | BuiltinTypeLoc bt ->
      Format.fprintf ff "%s"
        (string_of_builtin_type bt)
  | TypedefTypeLoc name ->
      Format.fprintf ff "%s"
        name
  | TypeOfExprTypeLoc expr ->
      Format.fprintf ff "typeof (%a)"
        pp_expr expr
  | TypeOfTypeLoc ty ->
      Format.fprintf ff "typeof (%a)"
        pp_tloc ty
  | ParenTypeLoc ty ->
      Format.fprintf ff "(%a)"
        pp_tloc ty
  | PointerTypeLoc ty ->
      Format.fprintf ff "%a ptr"
        pp_tloc ty
  | FunctionNoProtoTypeLoc ty ->
      Format.fprintf ff "? -> %a"
        pp_tloc ty
  | FunctionProtoTypeLoc (ty, args) ->
      Format.fprintf ff "(%a) -> %a"
        (Formatx.pp_list pp_decl) args
        pp_tloc ty
  | ConstantArrayTypeLoc (ty, size) ->
      Format.fprintf ff "%a[%d]"
        pp_tloc ty
        size
  | VariableArrayTypeLoc (ty, size) ->
      Format.fprintf ff "%a[%a]"
        pp_tloc ty
        pp_expr size
  | IncompleteArrayTypeLoc ty ->
      Format.fprintf ff "%a[]"
        pp_tloc ty
  | ElaboratedTypeLoc ty ->
      Format.fprintf ff "%a"
        pp_tloc ty
  | RecordTypeLoc (kind, name) ->
      Format.fprintf ff "%s %s"
        (string_of_tag_type_kind kind)
        (if name = "" then "<anonymous>" else name)
  | EnumTypeLoc name ->
      Format.pp_print_string ff
        (if name = "" then "<anonymous>" else name)

  | AtomicTypeLoc -> Format.pp_print_string ff "<AtomicTypeLoc>"
  | AttributedTypeLoc -> Format.pp_print_string ff "<AttributedTypeLoc>"
  | AutoTypeLoc -> Format.pp_print_string ff "<AutoTypeLoc>"
  | BlockPointerTypeLoc -> Format.pp_print_string ff "<BlockPointerTypeLoc>"
  | ComplexTypeLoc -> Format.pp_print_string ff "<ComplexTypeLoc>"
  | DecltypeTypeLoc -> Format.pp_print_string ff "<DecltypeTypeLoc>"
  | DependentNameTypeLoc -> Format.pp_print_string ff "<DependentNameTypeLoc>"
  | DependentSizedArrayTypeLoc -> Format.pp_print_string ff "<DependentSizedArrayTypeLoc>"
  | DependentSizedExtVectorTypeLoc -> Format.pp_print_string ff "<DependentSizedExtVectorTypeLoc>"
  | DependentTemplateSpecializationTypeLoc -> Format.pp_print_string ff "<DependentTemplateSpecializationTypeLoc>"
  | ExtVectorTypeLoc -> Format.pp_print_string ff "<ExtVectorTypeLoc>"
  | InjectedClassNameTypeLoc -> Format.pp_print_string ff "<InjectedClassNameTypeLoc>"
  | LValueReferenceTypeLoc -> Format.pp_print_string ff "<LValueReferenceTypeLoc>"
  | MemberPointerTypeLoc -> Format.pp_print_string ff "<MemberPointerTypeLoc>"
  | ObjCInterfaceTypeLoc -> Format.pp_print_string ff "<ObjCInterfaceTypeLoc>"
  | ObjCObjectTypeLoc -> Format.pp_print_string ff "<ObjCObjectTypeLoc>"
  | ObjCObjectPointerTypeLoc -> Format.pp_print_string ff "<ObjCObjectPointerTypeLoc>"
  | PackExpansionTypeLoc -> Format.pp_print_string ff "<PackExpansionTypeLoc>"
  | RValueReferenceTypeLoc -> Format.pp_print_string ff "<RValueReferenceTypeLoc>"
  | SubstTemplateTypeParmTypeLoc -> Format.pp_print_string ff "<SubstTemplateTypeParmTypeLoc>"
  | SubstTemplateTypeParmPackTypeLoc -> Format.pp_print_string ff "<SubstTemplateTypeParmPackTypeLoc>"
  | TemplateSpecializationTypeLoc -> Format.pp_print_string ff "<TemplateSpecializationTypeLoc>"
  | TemplateTypeParmTypeLoc -> Format.pp_print_string ff "<TemplateTypeParmTypeLoc>"
  | UnaryTransformTypeLoc -> Format.pp_print_string ff "<UnaryTransformTypeLoc>"
  | UnresolvedUsingTypeLoc -> Format.pp_print_string ff "<UnresolvedUsingTypeLoc>"
  | VectorTypeLoc -> Format.pp_print_string ff "<VectorTypeLoc>"

and pp_tloc ff tloc =
  pp_tloc_ ff tloc.tl


and pp_type_ ff = function
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
  | IncompleteArrayType ty ->
      Format.fprintf ff "%a[]"
        pp_type ty
  | ElaboratedType ty ->
      Format.fprintf ff "%a"
        pp_type ty
  | RecordType (kind, name) ->
      Format.fprintf ff "%s %s"
        (string_of_tag_type_kind kind)
        (if name = "" then "<anonymous>" else name)
  | EnumType name ->
      Format.pp_print_string ff
        (if name = "" then "<anonymous>" else name)
  | DecayedType (decayed, original) ->
      Format.fprintf ff "%a"
        pp_type decayed

  | AtomicType -> Format.pp_print_string ff "<AtomicType>"
  | AttributedType -> Format.pp_print_string ff "<AttributedType>"
  | AutoType -> Format.pp_print_string ff "<AutoType>"
  | BlockPointerType -> Format.pp_print_string ff "<BlockPointerType>"
  | ComplexType -> Format.pp_print_string ff "<ComplexType>"
  | DecltypeType -> Format.pp_print_string ff "<DecltypeType>"
  | DependentNameType -> Format.pp_print_string ff "<DependentNameType>"
  | DependentSizedArrayType -> Format.pp_print_string ff "<DependentSizedArrayType>"
  | DependentSizedExtVectorType -> Format.pp_print_string ff "<DependentSizedExtVectorType>"
  | DependentTemplateSpecializationType -> Format.pp_print_string ff "<DependentTemplateSpecializationType>"
  | ExtVectorType -> Format.pp_print_string ff "<ExtVectorType>"
  | InjectedClassNameType -> Format.pp_print_string ff "<InjectedClassNameType>"
  | LValueReferenceType -> Format.pp_print_string ff "<LValueReferenceType>"
  | MemberPointerType -> Format.pp_print_string ff "<MemberPointerType>"
  | ObjCInterfaceType -> Format.pp_print_string ff "<ObjCInterfaceType>"
  | ObjCObjectPointerType -> Format.pp_print_string ff "<ObjCObjectPointerType>"
  | ObjCObjectType -> Format.pp_print_string ff "<ObjCObjectType>"
  | PackExpansionType -> Format.pp_print_string ff "<PackExpansionType>"
  | RValueReferenceType -> Format.pp_print_string ff "<RValueReferenceType>"
  | SubstTemplateTypeParmPackType -> Format.pp_print_string ff "<SubstTemplateTypeParmPackType>"
  | SubstTemplateTypeParmType -> Format.pp_print_string ff "<SubstTemplateTypeParmType>"
  | TemplateSpecializationType -> Format.pp_print_string ff "<TemplateSpecializationType>"
  | TemplateTypeParmType -> Format.pp_print_string ff "<TemplateTypeParmType>"
  | UnaryTransformType -> Format.pp_print_string ff "<UnaryTransformType>"
  | UnresolvedUsingType -> Format.pp_print_string ff "<UnresolvedUsingType>"
  | VectorType -> Format.pp_print_string ff "<VectorType>"

and pp_type ff t =
  Format.fprintf ff "%a %a%s"
    pp_type_ t.t
    Formatx.(pp_list ~sep:(pp_sep "") pp_print_string)
      (List.map string_of_qualifier t.t_qual)
    (match t.t_aspace with
     | None -> ""
     | Some aspace -> " addr_space_" ^ string_of_int aspace)


and pp_decl_ ff = function
  | EmptyDecl ->
      Format.pp_print_string ff ";"
  | TranslationUnitDecl decls ->
      Formatx.pp_list ~sep:(Formatx.pp_sep "") pp_decl ff decls
  | TypedefDecl (ty, name) ->
      Format.fprintf ff "typedef %s : %a;"
        name
        pp_tloc ty
  | FunctionDecl (fd_type, fd_name, fd_body) ->
      Format.fprintf ff "@[<v2>%a@]%a"
        pp_named_arg (fd_name, fd_type)
        (pp_option pp_stmt) fd_body
  | VarDecl (ty, name, Some init) ->
      Format.fprintf ff "%a = %a"
        pp_named_arg (name, ty)
        pp_expr init
  | VarDecl (ty, name, None)
  | ParmVarDecl (ty, name) ->
      pp_named_arg ff (name, ty)
  | RecordDecl (name, []) ->
      Format.fprintf ff "struct %s@;"
        (if name = "" then "<anonymous>" else name)
  | RecordDecl (name, members) ->
      Format.fprintf ff "struct %s@\n@[<v2>{@,%a@]@\n};"
        (if name = "" then "<anonymous>" else name)
        (Formatx.pp_list ~sep:(Formatx.pp_sep "") pp_decl) members
  | FieldDecl (ty, name, bitwidth, init) ->
      Format.fprintf ff "%a;"
        pp_named_arg (name, ty)
  | EnumDecl (name, enumerators) ->
      Format.fprintf ff "enum %s { %a };"
        name
        (Formatx.pp_list ~sep:(Formatx.pp_sep "") pp_decl) enumerators
  | EnumConstantDecl (name, None) ->
      Format.fprintf ff "%s;"
        name
  | EnumConstantDecl (name, Some init) ->
      Format.fprintf ff "%s = %a;"
        name
        pp_expr init

  | AccessSpecDecl -> Format.pp_print_string ff "<AccessSpecDecl>"
  | BlockDecl -> Format.pp_print_string ff "<BlockDecl>"
  | CapturedDecl -> Format.pp_print_string ff "<CapturedDecl>"
  | ClassScopeFunctionSpecializationDecl -> Format.pp_print_string ff "<ClassScopeFunctionSpecializationDecl>"
  | ClassTemplateDecl -> Format.pp_print_string ff "<ClassTemplateDecl>"
  | FileScopeAsmDecl -> Format.pp_print_string ff "<FileScopeAsmDecl>"
  | FriendDecl -> Format.pp_print_string ff "<FriendDecl>"
  | FriendTemplateDecl -> Format.pp_print_string ff "<FriendTemplateDecl>"
  | FunctionTemplateDecl -> Format.pp_print_string ff "<FunctionTemplateDecl>"
  | ImportDecl -> Format.pp_print_string ff "<ImportDecl>"
  | IndirectFieldDecl -> Format.pp_print_string ff "<IndirectFieldDecl>"
  | LabelDecl -> Format.pp_print_string ff "<LabelDecl>"
  | LinkageSpecDecl -> Format.pp_print_string ff "<LinkageSpecDecl>"
  | MSPropertyDecl -> Format.pp_print_string ff "<MSPropertyDecl>"
  | NamespaceAliasDecl -> Format.pp_print_string ff "<NamespaceAliasDecl>"
  | NamespaceDecl -> Format.pp_print_string ff "<NamespaceDecl>"
  | NonTypeTemplateParmDecl -> Format.pp_print_string ff "<NonTypeTemplateParmDecl>"
  | ObjCCategoryDecl -> Format.pp_print_string ff "<ObjCCategoryDecl>"
  | ObjCCategoryImplDecl -> Format.pp_print_string ff "<ObjCCategoryImplDecl>"
  | ObjCCompatibleAliasDecl -> Format.pp_print_string ff "<ObjCCompatibleAliasDecl>"
  | ObjCImplementationDecl -> Format.pp_print_string ff "<ObjCImplementationDecl>"
  | ObjCInterfaceDecl -> Format.pp_print_string ff "<ObjCInterfaceDecl>"
  | ObjCMethodDecl -> Format.pp_print_string ff "<ObjCMethodDecl>"
  | ObjCPropertyDecl -> Format.pp_print_string ff "<ObjCPropertyDecl>"
  | ObjCPropertyImplDecl -> Format.pp_print_string ff "<ObjCPropertyImplDecl>"
  | ObjCProtocolDecl -> Format.pp_print_string ff "<ObjCProtocolDecl>"
  | OMPThreadPrivateDecl -> Format.pp_print_string ff "<OMPThreadPrivateDecl>"
  | StaticAssertDecl -> Format.pp_print_string ff "<StaticAssertDecl>"
  | TemplateTemplateParmDecl -> Format.pp_print_string ff "<TemplateTemplateParmDecl>"
  | TemplateTypeParmDecl -> Format.pp_print_string ff "<TemplateTypeParmDecl>"
  | TypeAliasDecl -> Format.pp_print_string ff "<TypeAliasDecl>"
  | TypeAliasTemplateDecl -> Format.pp_print_string ff "<TypeAliasTemplateDecl>"
  | UnresolvedUsingTypenameDecl -> Format.pp_print_string ff "<UnresolvedUsingTypenameDecl>"
  | UnresolvedUsingValueDecl -> Format.pp_print_string ff "<UnresolvedUsingValueDecl>"
  | UsingDecl -> Format.pp_print_string ff "<UsingDecl>"
  | UsingDirectiveDecl -> Format.pp_print_string ff "<UsingDirectiveDecl>"
  | UsingShadowDecl -> Format.pp_print_string ff "<UsingShadowDecl>"

and pp_decl ff decl =
  pp_decl_ ff decl.d


and pp_named_arg ff = function
  | ("", ty) ->
      pp_tloc ff ty
  | (name, ty) ->
      Format.fprintf ff "%s : %a"
        name
        pp_tloc ty
