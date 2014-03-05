let version = "20140120"

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

type predefined_ident = ClangBridge.predefined_ident =
  | PI_Func
  | PI_Function
  | PI_LFunction
  | PI_FuncDName
  | PI_PrettyFunction
  | PI_PrettyFunctionNoVirtual
  deriving (Show)

type tag_type_kind = ClangBridge.tag_type_kind =
  | TTK_Struct
  | TTK_Interface
  | TTK_Union
  | TTK_Class
  | TTK_Enum
  deriving (Show)

type elaborated_type_keyword = ClangBridge.elaborated_type_keyword =
  | ETK_Struct
  | ETK_Interface
  | ETK_Union
  | ETK_Class
  | ETK_Enum
  | ETK_Typename
  | ETK_None
  deriving (Show)

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


type unary_op = ClangBridge.unary_op =
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


type binary_op = ClangBridge.binary_op =
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


(*
type expr = ClangBridge.expr =
  | Unit of Clang.Unit.t
  | IntConst of Clang.IntConst.t	* int
  | BinaryOp of Clang.BinaryOp.t	* binary_op * expr * expr

and stmt = ClangBridge.stmt =
  | Skip of Clang.Skip.t
  | Print of Clang.Print.t		* expr
  | Block of Clang.Block.t		* stmt list
*)

type sloc = ClangBridge.sloc = {
  loc_s_filename : string;
  loc_s_line     : int;
  loc_s_column   : int;
  loc_e_filename : string;
  loc_e_line     : int;
  loc_e_column   : int;
} deriving (Show)

type designator = ClangBridge.designator =
  | FieldDesignator		of sloc * string
  | ArrayDesignator		of sloc * expr
  | ArrayRangeDesignator	of sloc * expr * expr

and expr = ClangBridge.expr =
  | UnimpExpr			of sloc * string

  | TypedExpr			of expr * ctyp

  | IntegerLiteral		of sloc * int
  | CharacterLiteral		of sloc * char
  | FloatingLiteral		of sloc * float
  | StringLiteral		of sloc * string
  | BinaryOperator		of sloc * binary_op * expr * expr
  | UnaryOperator		of sloc * unary_op * expr

  | DeclRefExpr			of sloc * (* name *)string
  | PredefinedExpr		of sloc * (* kind *)predefined_ident
  | ImplicitCastExpr		of sloc * expr
  | CStyleCastExpr		of sloc * type_loc * expr
  | CompoundLiteralExpr		of sloc * type_loc * (* init *)expr
  | ParenExpr			of sloc * expr
  | VAArgExpr			of sloc * (* sub *)expr * (* type *)type_loc
  | CallExpr			of sloc * (* callee *)expr * (* args *)expr list
  | MemberExpr			of sloc * (* base *)expr * (* member *)string * (* is_arrow *)bool
  | ConditionalOperator		of sloc * expr * expr * expr
  | DesignatedInitExpr		of sloc * designator list * expr
  | InitListExpr		of sloc * expr list
  | ImplicitValueInitExpr	of sloc
  | ArraySubscriptExpr		of sloc * expr * expr
  | StmtExpr			of sloc * stmt

  | SizeOfExpr			of sloc * expr
  | SizeOfType			of sloc * type_loc
  | AlignOfExpr			of sloc * expr
  | AlignOfType			of sloc * type_loc
  | VecStepExpr			of sloc * expr
  | VecStepType			of sloc * type_loc

and expr_ = ClangBridge.expr_ = {
  e : expr;
  e_sloc : sloc;
  e_type : ctyp;
}

and stmt = ClangBridge.stmt =
  | UnimpStmt			of sloc * string

  | NullStmt			of sloc
  | BreakStmt			of sloc
  | ContinueStmt		of sloc
  | LabelStmt			of sloc * string * stmt
  | CaseStmt			of sloc * expr * expr option * stmt
  | DefaultStmt			of sloc * stmt
  | GotoStmt			of sloc * string
  | ExprStmt			of (* no sloc *) expr
  | CompoundStmt		of sloc * stmt list
  | ReturnStmt			of sloc * expr option
  | IfStmt			of sloc * expr * stmt * stmt option
  | ForStmt			of sloc * (* init *)stmt option * (* cond *)expr option * (* incr *)expr option * (* body *)stmt
  | WhileStmt			of sloc * (* cond *)expr * (* body *)stmt
  | DoStmt			of sloc * (* body *)stmt * (* cond *)expr
  | SwitchStmt			of sloc * expr * stmt
  | DeclStmt			of sloc * decl list

and stmt_ = ClangBridge.stmt_ = {
  s : stmt;
  s_sloc : sloc;
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

and type_loc = ClangBridge.type_loc = {
  tl : type_loc_;
  tl_sloc : sloc;
}

and ctyp = ClangBridge.ctyp =
  | UnimpType			of string

  | BuiltinType			of builtin_type
  | TypeOfExprType		of expr
  | TypeOfType			of ctyp
  | ParenType			of ctyp
  | QualifiedType		of (* unqualified *)ctyp * (* qualifiers *)qualifier list * (* address-space *)int option
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

and decl = ClangBridge.decl = {
  d : decl_;
  d_sloc : sloc;
}

  (* All of the above derive Show. *)
  deriving (Show)
