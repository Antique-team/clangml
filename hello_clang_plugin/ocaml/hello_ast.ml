let version = "20140120"

type qualifier =
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

type predefined_ident =
  | PI_Func
  | PI_Function
  | PI_LFunction
  | PI_FuncDName
  | PI_PrettyFunction
  | PI_PrettyFunctionNoVirtual

type tag_type_kind =
  | TTK_Struct
  | TTK_Interface
  | TTK_Union
  | TTK_Class
  | TTK_Enum

type elaborated_type_keyword =
  | ETK_Struct
  | ETK_Interface
  | ETK_Union
  | ETK_Class
  | ETK_Enum
  | ETK_Typename
  | ETK_None

type builtin_type =
  | BT_Void
  | BT_Bool
  | BT_Char_U
  | BT_UChar
  | BT_WChar_U
  | BT_Char16
  | BT_Char32
  | BT_UShort
  | BT_UInt
  | BT_ULong
  | BT_ULongLong
  | BT_UInt128
  | BT_Char_S
  | BT_SChar
  | BT_WChar_S
  | BT_Short
  | BT_Int
  | BT_Long
  | BT_LongLong
  | BT_Int128
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


type unary_op =
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


type binary_op =
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


(*
type expr =
  | Unit of Clang.Unit.t
  | IntConst of Clang.IntConst.t	* int
  | BinaryOp of Clang.BinaryOp.t	* binary_op * expr * expr

and stmt =
  | Skip of Clang.Skip.t
  | Print of Clang.Print.t		* expr
  | Block of Clang.Block.t		* stmt list
*)

type designator =
  | FieldDesignator		of string
  | ArrayDesignator		of expr
  | ArrayRangeDesignator	of expr * expr

and expr =
  | UnimpExpr			of string

  | IntegerLiteral		of int
  | CharacterLiteral		of char
  | FloatingLiteral		of float
  | StringLiteral		of string
  | BinaryOperator		of binary_op * expr * expr
  | UnaryOperator		of unary_op * expr

  | DeclRefExpr			of (* name *)string
  | PredefinedExpr		of (* kind *)predefined_ident
  | ImplicitCastExpr		of expr
  | CStyleCastExpr		of type_loc * expr
  | CompoundLiteralExpr		of type_loc * (* init *)expr
  | ParenExpr			of expr
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

and stmt =
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

and type_loc =
  | UnimpTypeLoc		of string

  | BuiltinTypeLoc		of builtin_type
  | TypeOfExprTypeLoc		of expr
  | TypeOfTypeLoc		of type_loc
  | QualifiedTypeLoc		of type_loc * qualifier list * int option
  | TypedefTypeLoc		of (* name *)string
  | PointerTypeLoc		of (* pointee *)type_loc
  | FunctionNoProtoTypeLoc	of (* result *)type_loc
  | FunctionProtoTypeLoc	of (* result *)type_loc * decl list
  | ConstantArrayTypeLoc	of (* member-type *)type_loc * (* size *)int
  | VariableArrayTypeLoc	of (* member-type *)type_loc * (* size *)expr
  | IncompleteArrayTypeLoc	of (* member-type *)type_loc
  | ElaboratedTypeLoc		of (* named-type *)type_loc
  | EnumTypeLoc			of (* name *)string
  | RecordTypeLoc		of (* kind *)tag_type_kind * (* name *)string

and decl =
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
