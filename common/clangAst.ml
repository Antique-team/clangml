let version = "$Id$"

(* clang/AST/Type.h *)
type qualifier = ClangBridge.qualifier =
  (* CVR *)
  | TQ_Const
  | TQ_Volatile
  | TQ_Restrict
  (* ObjCGC *)
  | TQ_Weak
  | TQ_Strong
  (* ObjCLifetime *)
  | TQ_OCL_ExplicitNone
  | TQ_OCL_Strong
  | TQ_OCL_Weak
  | TQ_OCL_Autoreleasing
  deriving (Show)


(* clang/AST/Expr.h *)
type predefined_expr = ClangBridge.predefined_expr =
  | PE_Func
  | PE_Function
  | PE_LFunction
  | PE_FuncDName
  | PE_PrettyFunction
  | PE_PrettyFunctionNoVirtual
  deriving (Show)


(* clang/AST/Type.h *)
type tag_type_kind = ClangBridge.tag_type_kind =
  | TTK_Struct
  | TTK_Interface
  | TTK_Union
  | TTK_Class
  | TTK_Enum
  deriving (Show)


(* clang/AST/Type.h *)
type elaborated_type_keyword = ClangBridge.elaborated_type_keyword =
  | ETK_Struct
  | ETK_Interface
  | ETK_Union
  | ETK_Class
  | ETK_Enum
  | ETK_Typename
  | ETK_None
  deriving (Show)


(* clang/AST/OperationKinds.h *)
type cast_kind = ClangBridge.cast_kind =
  | CK_Dependent
  | CK_BitCast
  | CK_LValueBitCast
  | CK_LValueToRValue
  | CK_NoOp
  | CK_BaseToDerived
  | CK_DerivedToBase
  | CK_UncheckedDerivedToBase
  | CK_Dynamic
  | CK_ToUnion
  | CK_ArrayToPointerDecay
  | CK_FunctionToPointerDecay
  | CK_NullToPointer
  | CK_NullToMemberPointer
  | CK_BaseToDerivedMemberPointer
  | CK_DerivedToBaseMemberPointer
  | CK_MemberPointerToBoolean
  | CK_ReinterpretMemberPointer
  | CK_UserDefinedConversion
  | CK_ConstructorConversion
  | CK_IntegralToPointer
  | CK_PointerToIntegral
  | CK_PointerToBoolean
  | CK_ToVoid
  | CK_VectorSplat
  | CK_IntegralCast
  | CK_IntegralToBoolean
  | CK_IntegralToFloating
  | CK_FloatingToIntegral
  | CK_FloatingToBoolean
  | CK_FloatingCast
  | CK_CPointerToObjCPointerCast
  | CK_BlockPointerToObjCPointerCast
  | CK_AnyPointerToBlockPointerCast
  | CK_ObjCObjectLValueCast
  | CK_FloatingRealToComplex
  | CK_FloatingComplexToReal
  | CK_FloatingComplexToBoolean
  | CK_FloatingComplexCast
  | CK_FloatingComplexToIntegralComplex
  | CK_IntegralRealToComplex
  | CK_IntegralComplexToReal
  | CK_IntegralComplexToBoolean
  | CK_IntegralComplexCast
  | CK_IntegralComplexToFloatingComplex
  | CK_ARCProduceObject
  | CK_ARCConsumeObject
  | CK_ARCReclaimReturnedObject
  | CK_ARCExtendBlockObject
  | CK_AtomicToNonAtomic
  | CK_NonAtomicToAtomic
  | CK_CopyAndAutoreleaseBlockObject
  | CK_BuiltinFnToFnPtr
  | CK_ZeroToOCLEvent
  deriving (Show)


(* clang/AST/OperationKinds.h *)
type unary_operator = ClangBridge.unary_operator =
  | UO_PostInc	(* [C99 6.5.2.4] Postfix increment and decrement *)
  | UO_PostDec
  | UO_PreInc	(* [C99 6.5.3.1] Prefix increment and decrement *)
  | UO_PreDec
  | UO_AddrOf	(* [C99 6.5.3.2] Address and indirection *)
  | UO_Deref
  | UO_Plus	(* [C99 6.5.3.3] Unary arithmetic *)
  | UO_Minus
  | UO_Not
  | UO_LNot
  | UO_Real	(* "__real expr"/"__imag expr" Extension. *)
  | UO_Imag
  | UO_Extension(* __extension__ marker. *)
  deriving (Show)


(* clang/AST/OperationKinds.h *)
type binary_operator = ClangBridge.binary_operator =
  | BO_PtrMemD	(* [C++ 5.5] Pointer-to-member operators. *)
  | BO_PtrMemI
  | BO_Mul	(* [C99 6.5.5] Multiplicative operators. *)
  | BO_Div
  | BO_Rem
  | BO_Add	(* [C99 6.5.6] Additive operators. *)
  | BO_Sub
  | BO_Shl	(* [C99 6.5.7] Bitwise shift operators. *)
  | BO_Shr
  | BO_LT	(* [C99 6.5.8] Relational operators. *)
  | BO_GT
  | BO_LE
  | BO_GE
  | BO_EQ	(* [C99 6.5.9] Equality operators. *)
  | BO_NE
  | BO_And	(* [C99 6.5.10] Bitwise AND operator. *)
  | BO_Xor	(* [C99 6.5.11] Bitwise XOR operator. *)
  | BO_Or	(* [C99 6.5.12] Bitwise OR operator. *)
  | BO_LAnd	(* [C99 6.5.13] Logical AND operator. *)
  | BO_LOr	(* [C99 6.5.14] Logical OR operator. *)
  | BO_Assign	(* [C99 6.5.16] Assignment operators. *)
  | BO_MulAssign
  | BO_DivAssign
  | BO_RemAssign
  | BO_AddAssign
  | BO_SubAssign
  | BO_ShlAssign
  | BO_ShrAssign
  | BO_AndAssign
  | BO_OrAssign
  | BO_XorAssign
  | BO_Comma	(* [C99 6.5.17] Comma operator. *)
  deriving (Show)


(* clang/AST/BuiltinTypes.def *)
type builtin_type = ClangBridge.builtin_type =
  | BT_Void
  | BT_Bool

  | BT_Char_S
  | BT_Char_U
  | BT_SChar
  | BT_UChar
  | BT_WChar_U
  | BT_WChar_S
  | BT_Char16
  | BT_Char32

  | BT_Short
  | BT_UShort
  | BT_Int
  | BT_UInt
  | BT_Long
  | BT_ULong
  | BT_LongLong
  | BT_ULongLong
  | BT_Int128
  | BT_UInt128

  | BT_Half
  | BT_Float
  | BT_Double
  | BT_LongDouble

  | BT_NullPtr

  | BT_ObjCId
  | BT_ObjCClass
  | BT_ObjCSel

  | BT_OCLImage1d
  | BT_OCLImage1dArray
  | BT_OCLImage1dBuffer
  | BT_OCLImage2d
  | BT_OCLImage2dArray
  | BT_OCLImage3d
  | BT_OCLSampler
  | BT_OCLEvent

  | BT_Dependent
  | BT_Overload
  | BT_BoundMember
  | BT_PseudoObject
  | BT_UnknownAny
  | BT_BuiltinFn
  | BT_ARCUnbridgedCast
  deriving (Show)


type sloc = ClangBridge.sloc = {
  loc_s_filename : string;
  loc_s_line     : int;
  loc_s_column   : int;
  loc_e_filename : string;
  loc_e_line     : int;
  loc_e_column   : int;
} deriving (Show)


type designator = ClangBridge.designator = {
  dr : designator_;
  dr_sloc : sloc;
}

and designator_ = ClangBridge.designator_ =
  | FieldDesignator		of string
  | ArrayDesignator		of expr
  | ArrayRangeDesignator	of expr * expr


and expr = ClangBridge.expr = {
  e      : expr_;
  e_cref : expr Clang.t;
  e_sloc : sloc;
  e_type : ctyp;
}

and expr_ = ClangBridge.expr_ =
  | UnimpExpr			of string

  | IntegerLiteral		of int
  | CharacterLiteral		of char
  | FloatingLiteral		of float
  | StringLiteral		of string
  | BinaryOperator		of binary_operator * expr * expr
  | UnaryOperator		of unary_operator * expr

  | DeclRefExpr			of (* name *)string
  | PredefinedExpr		of (* kind *)predefined_expr
  | ImplicitCastExpr		of cast_kind * expr
  | CStyleCastExpr		of cast_kind * type_loc * expr
  | CompoundLiteralExpr		of type_loc * (* init *)expr
  | ParenExpr			of expr
  | VAArgExpr			of (* sub *)expr * (* type *)type_loc
  | CallExpr			of (* callee *)expr * (* args *)expr list
  | MemberExpr			of (* base *)expr * (* member *)string * (* is_arrow *)bool
  | ConditionalOperator		of expr * expr * expr
  | DesignatedInitExpr		of designator list * expr
  | InitListExpr		of expr list
  | ImplicitValueInitExpr
  | ArraySubscriptExpr		of expr * expr
  | StmtExpr			of stmt

  | SizeOfExpr			of expr
  | SizeOfType			of type_loc
  | AlignOfExpr			of expr
  | AlignOfType			of type_loc
  | VecStepExpr			of expr
  | VecStepType			of type_loc


and stmt = ClangBridge.stmt = {
  s      : stmt_;
  s_cref : stmt Clang.t;
  s_sloc : sloc;
}

and stmt_ = ClangBridge.stmt_ =
  | UnimpStmt			of string

  | NullStmt
  | BreakStmt
  | ContinueStmt
  | LabelStmt			of string * stmt
  | CaseStmt			of expr * expr option * stmt
  | DefaultStmt			of stmt
  | GotoStmt			of string
  | ExprStmt			of expr
  | CompoundStmt		of stmt list
  | ReturnStmt			of expr option
  | IfStmt			of expr * stmt * stmt option
  | ForStmt			of (* init *)stmt option * (* cond *)expr option * (* incr *)expr option * (* body *)stmt
  | WhileStmt			of (* cond *)expr * (* body *)stmt
  | DoStmt			of (* body *)stmt * (* cond *)expr
  | SwitchStmt			of expr * stmt
  | DeclStmt			of decl list


and type_loc = ClangBridge.type_loc = {
  tl      : type_loc_;
  tl_cref : type_loc Clang.t;
  tl_sloc : sloc;
}

and type_loc_ = ClangBridge.type_loc_ =
  | UnimpTypeLoc		of string

  | BuiltinTypeLoc		of builtin_type
  | TypeOfExprTypeLoc		of expr
  | TypeOfTypeLoc		of type_loc
  | ParenTypeLoc		of type_loc
  | QualifiedTypeLoc		of type_loc * qualifier list * int option
  | TypedefTypeLoc		of (* name *)string
  | PointerTypeLoc		of (* pointee *)type_loc
  | FunctionNoProtoTypeLoc	of (* result *)type_loc
  | FunctionProtoTypeLoc	of (* result *)type_loc * (* args *)decl list
  | ConstantArrayTypeLoc	of (* member-type *)type_loc * (* size *)int
  | VariableArrayTypeLoc	of (* member-type *)type_loc * (* size *)expr
  | IncompleteArrayTypeLoc	of (* member-type *)type_loc
  | ElaboratedTypeLoc		of (* named-type *)type_loc
  | EnumTypeLoc			of (* name *)string
  | RecordTypeLoc		of (* kind *)tag_type_kind * (* name *)string


and ctyp = ClangBridge.ctyp = {
  t        : ctyp_;
  t_cref   : ctyp Clang.t;
  t_qual   : qualifier list;
  t_aspace : int option;
}

and ctyp_ = ClangBridge.ctyp_ =
  | UnimpType			of string

  | BuiltinType			of builtin_type
  | TypeOfExprType		of expr
  | TypeOfType			of ctyp
  | ParenType			of ctyp
  | TypedefType			of (* name *)string
  | PointerType			of (* pointee *)ctyp
  | FunctionNoProtoType		of (* result *)ctyp
  | FunctionProtoType		of (* result *)ctyp * ctyp list
  | ConstantArrayType		of (* member-type *)ctyp * (* size *)int
  | VariableArrayType		of (* member-type *)ctyp * (* size *)expr
  | IncompleteArrayType		of (* member-type *)ctyp
  | ElaboratedType		of (* named-type *)ctyp
  | EnumType			of (* name *)string
  | RecordType			of (* kind *)tag_type_kind * (* name *)string
  | DecayedType			of (* decayed *)ctyp * (* original *)ctyp


and decl = ClangBridge.decl = {
  d      : decl_;
  d_cref : decl Clang.t;
  d_sloc : sloc;
}

and decl_ = ClangBridge.decl_ =
  | UnimpDecl			of string

  | EmptyDecl
  | TranslationUnitDecl		of decl list
  | FunctionDecl		of (* type *)type_loc * (* name *)string * (* body *)stmt option
  | TypedefDecl			of (* type *)type_loc * (* name *)string
  | VarDecl			of (* type *)type_loc * (* name *)string * (* init *)expr option
  | ParmVarDecl			of (* type *)type_loc * (* name *)string
  | RecordDecl			of (* name *)string * (* members *)decl list
  | FieldDecl			of (* type *)type_loc * (* name *)string * (* bitwidth *)expr option * (* initialiser *)expr option
  | EnumDecl			of (* name *)string * (* enumerators *)decl list
  | EnumConstantDecl		of (* name *)string * (* init *)expr option

  (* All of the above derive Show. *)
  deriving (Show)
