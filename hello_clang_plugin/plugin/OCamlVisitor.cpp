#include "OCamlVisitor.h"
#include "dynamic_stack.h"
#include "trace.h"

using namespace hello_cpp;


struct OCamlVisitor
  : clang::RecursiveASTVisitor<OCamlVisitor>
{
private:
  typedef clang::RecursiveASTVisitor<OCamlVisitor> Base;

  dynamic_stack stack;


  // Traverse a clang object, but keep it on the stack.
  template<typename T, bool (OCamlVisitor::*Fun) (T p)>
  void traverse (T p)
  {
    // Require stack to increase by exactly 1 (one value was pushed).
    size_t size_before = stack.size ();
    (this->*Fun) (p);
    size_t size_after = stack.size ();

    assert (size_after == size_before + 1);
  }


  // Overloads to call appropriate traversal functions.
  void traverse (clang::Stmt *S)
  { traverse<clang::Stmt *, &OCamlVisitor::TraverseStmt> (S); }

  void traverse (clang::TypeLoc TL)
  { traverse<clang::TypeLoc, &OCamlVisitor::TraverseTypeLoc> (TL); }


  // May take a pointer or an object.
  template<typename T>
  dynamic_stack::element must_traverse (T p)
  {
    // Require clang pointer to be non-null.
    // In case of TypeLoc, require the contained Type pointer
    // to be non-null.
    assert (p);
    // Traverse, which does not pop.
    traverse (p);
    // Now, get the bridge pointer.
    dynamic_stack::element result = stack.pop ();
    // Require OCaml bridge pointer to be non-null.
    assert (result);
    return result;
  }

  // Return null if p is null, otherwise call an appropriate traversal
  // function and return its result.
  template<typename T>
  dynamic_stack::element maybe_traverse (T p)
  {
    if (!p)
      return dynamic_stack::element { };
    return must_traverse (p);
  }


  /****************************************************
   * Helpers
   */

  // works for NameDecls and DeclRefExpr
  template<typename DeclT>
  static clang::StringRef getName (DeclT const *D)
  {
    clang::IdentifierInfo *info = D->getNameInfo ().getName ().getAsIdentifierInfo ();
    assert (info);
    return info->getName ();
  }


public:
  /****************************************************
   * Unary/binary operators
   */

// Operator lists taken from <clang/AST/RecursiveASTVisitor.h>

// All unary operators.
#define UNARYOP_LIST()                          \
  OPERATOR(PostInc)   OPERATOR(PostDec)         \
  OPERATOR(PreInc)    OPERATOR(PreDec)          \
  OPERATOR(AddrOf)    OPERATOR(Deref)           \
  OPERATOR(Plus)      OPERATOR(Minus)           \
  OPERATOR(Not)       OPERATOR(LNot)            \
  OPERATOR(Real)      OPERATOR(Imag)            \
  OPERATOR(Extension)

// All binary operators (excluding compound assign operators).
#define BINOP_LIST() \
  OPERATOR(PtrMemD)              OPERATOR(PtrMemI)    \
  OPERATOR(Mul)   OPERATOR(Div)  OPERATOR(Rem)        \
  OPERATOR(Add)   OPERATOR(Sub)  OPERATOR(Shl)        \
  OPERATOR(Shr)                                       \
                                                      \
  OPERATOR(LT)    OPERATOR(GT)   OPERATOR(LE)         \
  OPERATOR(GE)    OPERATOR(EQ)   OPERATOR(NE)         \
  OPERATOR(And)   OPERATOR(Xor)  OPERATOR(Or)         \
  OPERATOR(LAnd)  OPERATOR(LOr)                       \
                                                      \
  OPERATOR(Assign)                                    \
  OPERATOR(Comma)

// All compound assign operators.
#define CAO_LIST()                                                      \
  OPERATOR(Mul) OPERATOR(Div) OPERATOR(Rem) OPERATOR(Add) OPERATOR(Sub) \
  OPERATOR(Shl) OPERATOR(Shr) OPERATOR(And) OPERATOR(Or)  OPERATOR(Xor)


#define OPERATOR(OP)							\
  bool TraverseUnary##OP (clang::UnaryOperator *S)			\
  {									\
    ptr<Expr> sub = must_traverse (S->getSubExpr ());			\
    stack.push (mkUnaryOperator (UO_##OP, sub));			\
    return true;							\
  }

  UNARYOP_LIST ()
#undef OPERATOR


#define OPERATOR(OP)							\
  bool TraverseBin##OP (clang::BinaryOperator *S)			\
  {									\
    ptr<Expr> lhs = must_traverse (S->getLHS ());			\
    ptr<Expr> rhs = must_traverse (S->getRHS ());			\
    stack.push (mkBinaryOperator (BO_##OP, lhs, rhs));			\
    return true;							\
  }

  BINOP_LIST ()
#undef OPERATOR


#define OPERATOR(OP)							\
  bool TraverseBin##OP##Assign (clang::CompoundAssignOperator *S)	\
  {									\
    ptr<Expr> lhs = must_traverse (S->getLHS ());			\
    ptr<Expr> rhs = must_traverse (S->getRHS ());			\
    stack.push (mkBinaryOperator (BO_##OP##Assign, lhs, rhs));		\
    return true;							\
  }

  CAO_LIST ()
#undef OPERATOR

#undef UNARYOP_LIST
#undef BINOP_LIST
#undef CAO_LIST


  /****************************************************
   * Literals
   */

#define INTEGERLITERAL(CLASS, BASE)
  bool TraverseIntegerLiteral (clang::IntegerLiteral *lit)
  {
    stack.push (mkIntegerLiteral
                (lit->getValue ().getSExtValue ()));

    return true;
  }


#define CHARACTERLITERAL(CLASS, BASE)
  bool TraverseCharacterLiteral (clang::CharacterLiteral *lit)
  {
    stack.push (mkCharacterLiteral
                (lit->getValue ()));

    return true;
  }


#define FLOATINGLITERAL(CLASS, BASE)
  bool TraverseFloatingLiteral (clang::FloatingLiteral *lit)
  {
    stack.push (mkFloatingLiteral
                (lit->getValue ().convertToDouble ()));

    return true;
  }


#define STRINGLITERAL(CLASS, BASE)
  bool TraverseStringLiteral (clang::StringLiteral *lit)
  {
    stack.push (mkStringLiteral
                (lit->getString ()));

    return true;
  }


  /****************************************************
   * Expressions
   */

#define DECLREFEXPR(CLASS, BASE)
  bool TraverseDeclRefExpr (clang::DeclRefExpr *S)
  {
    TRACE;

    clang::StringRef name = getName (S);
    stack.push (mkDeclRefExpr (name));

    return true;
  }


#define IMPLICITCASTEXPR(CLASS, BASE)
  bool TraverseImplicitCastExpr (clang::ImplicitCastExpr *S)
  {
    TRACE;

    ptr<Expr> inner = must_traverse (S->getSubExpr ());

    stack.push (mkImplicitCastExpr (inner));

    return true;
  }


#define PARENEXPR(CLASS, BASE)
  bool TraverseParenExpr (clang::ParenExpr *S)
  {
    TRACE;

    ptr<Expr> inner = must_traverse (S->getSubExpr ());

    stack.push (mkParenExpr (inner));

    return true;
  }

  /****************************************************
   * Statements
   */

  // Simple forward so passing it as template argument in `maybe' works.
  bool TraverseStmt (clang::Stmt *S)
  {
    return Base::TraverseStmt (S);
  }

#define NULLSTMT(CLASS, BASE)
  bool TraverseNullStmt (clang::NullStmt *S)
  {
    TRACE;

    stack.push (mkNullStmt ());

    return true;
  }


#define BREAKSTMT(CLASS, BASE)
  bool TraverseBreakStmt (clang::BreakStmt *S)
  {
    TRACE;

    stack.push (mkBreakStmt ());

    return true;
  }


#define CONTINUESTMT(CLASS, BASE)
  bool TraverseContinueStmt (clang::ContinueStmt *S)
  {
    TRACE;

    stack.push (mkContinueStmt ());

    return true;
  }


#define IFSTMT(CLASS, BASE)
  bool TraverseIfStmt (clang::IfStmt *S)
  {
    TRACE;

    ptr<Expr> cond = must_traverse (S->getCond ());
    ptr<Stmt> thenBranch = must_traverse (S->getThen ());
    option<Stmt> elseBranch = maybe_traverse (S->getElse ());

    stack.push (mkIfStmt (cond, thenBranch, elseBranch));

    return true;
  }


#define FORSTMT(CLASS, BASE)
  bool TraverseForStmt (clang::ForStmt *S)
  {
    TRACE;

    option<Stmt> init = maybe_traverse (S->getInit ());
    option<Expr> cond = maybe_traverse (S->getCond ());
    option<Expr> inc = maybe_traverse (S->getInc ());
    ptr<Stmt> body = must_traverse (S->getBody ());

    stack.push (mkForStmt (init, cond, inc, body));

    return true;
  }


#define SWITCHSTMT(CLASS, BASE)
  bool TraverseSwitchStmt (clang::SwitchStmt *S)
  {
    TRACE;

    ptr<Expr> cond = must_traverse (S->getCond ());
    ptr<Stmt> body = must_traverse (S->getBody ());

    stack.push (mkSwitchStmt (cond, body));

    return true;
  }


#define CASESTMT(CLASS, BASE)
  bool TraverseCaseStmt (clang::CaseStmt *S)
  {
    TRACE;

    ptr<Expr> lhs = must_traverse (S->getLHS ());
    option<Expr> rhs = maybe_traverse (S->getRHS ());
    ptr<Stmt> sub = must_traverse (S->getSubStmt ());

    stack.push (mkCaseStmt (lhs, rhs, sub));

    return true;
  }


#define RETURNSTMT(CLASS, BASE)
  bool TraverseReturnStmt (clang::ReturnStmt *S)
  {
    TRACE;

    option<Expr> expr = maybe_traverse (S->getRetValue ());

    stack.push (mkReturnStmt (expr));

    return true;
  }


#define COMPOUNDSTMT(CLASS, BASE)
  bool TraverseCompoundStmt (clang::CompoundStmt *S)
  {
    TRACE;

    stack.push_mark ();
    for (clang::Stmt *sub : S->children ())
      // Traverse, but do not pop yet.
      traverse (sub);
    std::vector<ptr<Stmt>> stmts = stack.pop_marked ();

    stack.push (mkCompoundStmt (stmts));

    return true;
  }


#define DECLSTMT(CLASS, BASE)
  bool TraverseDeclStmt (clang::DeclStmt *S)
  {
    TRACE;
    Base::TraverseDeclStmt (S);

    ptr<Decl> decl = stack.pop ();
    stack.push (mkDeclStmt (decl));

    return true;
  }


#define IGNORE_ADT(CLASS, VAR)					\
    stack.push_mark ();						\
    Base::Traverse##CLASS (VAR);				\
    /* Drop everything made by previous calls. */		\
    size_t marker = stack.pop_mark ();				\
    while (marker--) stack.pop ()


#define UNIMP(TYPE, CLASS)					\
  bool Traverse##CLASS (clang::CLASS *N)			\
  {								\
    TRACE;							\
    IGNORE_ADT (CLASS, N);					\
    stack.push (mkUnimp##TYPE (#CLASS));			\
    return true;						\
  }


#define ABSTRACT_STMT(STMT)
#define STMT(CLASS, BASE)	UNIMP (Stmt, CLASS)
#define EXPR(CLASS, BASE)	UNIMP (Expr, CLASS)
#include <clang/AST/StmtNodes.inc>


  /****************************************************
   * Types
   */

#define ABSTRACT_TYPE(CLASS, BASE)
#define TYPE(CLASS, BASE)					\
  bool Traverse##CLASS##Type (clang::CLASS##Type *type)		\
  {								\
    TRACE;							\
    Base::Traverse##CLASS##Type (type);				\
    return true;						\
  }
#include <clang/AST/TypeNodes.def>


  /****************************************************
   * TypeLocs
   */

  bool TraverseTypeLoc (clang::TypeLoc TL)
  {
    TRACE;
    stack.push_mark ();
    Base::TraverseTypeLoc (TL);
    size_t marker = stack.pop_mark ();
    if (marker == 0)
      {
        printf ("WARNING: %s creates dummy TypeLoc, as derived function did not produce any\n",
                __func__);
        stack.push (mkBuiltinTypeLoc (BT_Void));
      }
    else if (marker > 1)
      {
        ptr<TypeLoc> mostRecent = stack.pop ();
        printf ("WARNING: %s drops all but most recent (out of %lu) TypeLoc\n",
                __func__, marker);
        // Keep the last one
        while (--marker) stack.pop ();
        stack.push (mostRecent);
      }
    return true;
  }

  static BuiltinType translate_builtin_type (clang::BuiltinType::Kind kind)
  {
    switch (kind)
      {
#define BUILTIN_TYPE(Id, SingletonId)	\
      case clang::BuiltinType::Id:	\
        return BT_##Id;
#include <clang/AST/BuiltinTypes.def>
      }
    throw std::runtime_error ("invalid builtin type");
  }

#if 1
  bool TraverseBuiltinTypeLoc (clang::BuiltinTypeLoc TL)
  {
    TRACE;
    Base::TraverseBuiltinTypeLoc (TL);

    BuiltinType bt = translate_builtin_type (TL.getTypePtr ()->getKind ());

    stack.push (mkBuiltinTypeLoc (bt));

    return true;
  }

  bool TraverseConstantArrayTypeLoc (clang::ConstantArrayTypeLoc TL)
  {
    TRACE;
    Base::TraverseConstantArrayTypeLoc (TL);

    ptr<TypeLoc> inner = stack.pop ();
    uint64_t size = TL.getTypePtr ()->getSize ().getZExtValue ();

    stack.push (mkConstantArrayTypeLoc (inner, size));

    return true;
  }

  bool TraversePointerTypeLoc (clang::PointerTypeLoc TL)
  {
    TRACE;
    Base::TraversePointerTypeLoc (TL);

    ptr<TypeLoc> inner = stack.pop ();
    stack.push (mkPointerTypeLoc (inner));

    return true;
  }

  bool TraverseFunctionNoProtoTypeLoc (clang::FunctionNoProtoTypeLoc TL)
  {
    TRACE;
    TraverseTypeLoc (TL.getResultLoc ());
    ptr<TypeLoc> result = stack.pop ();

    stack.push (mkFunctionNoProtoTypeLoc (result));

    return true;
  }

  bool TraverseFunctionProtoTypeLoc (clang::FunctionProtoTypeLoc TL)
  {
    TRACE;

    TraverseTypeLoc (TL.getResultLoc ());
    ptr<TypeLoc> result = stack.pop ();

    clang::FunctionProtoType const *T = TL.getTypePtr ();

    stack.push_mark ();

    for (unsigned I = 0, E = TL.getNumArgs (); I != E; ++I)
      {
        clang::ParmVarDecl *Arg = TL.getArg (I);
        assert (Arg);
        TraverseDecl (Arg);
      }

    std::vector<ptr<Decl>> args = stack.pop_marked ();

    // TODO: exceptions

    stack.push (mkFunctionProtoTypeLoc (result, args));

    return true;
  }

  bool TraverseTypedefTypeLoc (clang::TypedefTypeLoc TL)
  {
    TRACE;
    Base::TraverseTypedefTypeLoc (TL);

    clang::StringRef name = TL.getTypedefNameDecl ()->getName ();
    stack.push (mkTypedefTypeLoc (name));

    return true;
  }

#else
#define ABSTRACT_TYPELOC(CLASS, BASE)
#define TYPELOC(CLASS, BASE)					\
  bool Traverse##CLASS##TypeLoc (clang::CLASS##TypeLoc TL)	\
  {								\
    TRACE;							\
    IGNORE_ADT (CLASS##TypeLoc, TL);				\
    /* Default to void type. */					\
    stack.push (mkBuiltinTypeLoc (BT_Void));			\
    return true;						\
  }
#include <clang/AST/TypeLocNodes.def>
#endif


  /****************************************************
   * Declarations
   */

#define FUNCTION(CLASS, BASE)
  bool TraverseFunctionDecl (clang::FunctionDecl *D)
  {
    TRACE;

    // TODO: what are these? probably irrelevant in C.
    TraverseNestedNameSpecifierLoc (D->getQualifierLoc ());
    TraverseDeclarationNameInfo (D->getNameInfo ());

    // Function type, including parameters.
    clang::TypeSourceInfo *TSI = D->getTypeSourceInfo ();
    assert (TSI);
    TraverseTypeLoc (TSI->getTypeLoc ());
    ptr<TypeLoc> type = stack.pop ();

    // Function body, or None.
    option<Stmt> body;
    if (D->isThisDeclarationADefinition ())
      body = must_traverse (D->getBody ());

    // TODO: Constructor initialisers.

    // Function name.
    clang::StringRef name = getName (D);

    stack.push (mkFunctionDecl (type, name, body));

    return true;
  }


#define TYPEDEF(CLASS, BASE)
  bool TraverseTypedefDecl (clang::TypedefDecl *D)
  {
    TRACE;
    Base::TraverseTypedefDecl (D);

    ptr<TypeLoc> type = stack.pop ();
    clang::StringRef name = D->getName ();

    stack.push (mkTypedefDecl (type, name));

    return true;
  }


#define PARMVAR(CLASS, BASE)
  bool TraverseParmVarDecl (clang::ParmVarDecl *D)
  {
    TRACE;

    TraverseNestedNameSpecifierLoc (D->getQualifierLoc ());

    clang::TypeSourceInfo *TSI = D->getTypeSourceInfo ();
    assert (TSI);
    TraverseTypeLoc (TSI->getTypeLoc ());
    ptr<TypeLoc> type = stack.pop ();

    clang::StringRef name = D->getName ();

    stack.push (mkParmVarDecl (type, name));

    return true;
  }


#define VAR(CLASS, BASE)
  bool TraverseVarDecl (clang::VarDecl *D)
  {
    TRACE;

    TraverseNestedNameSpecifierLoc (D->getQualifierLoc ());

    clang::TypeSourceInfo *TSI = D->getTypeSourceInfo ();
    assert (TSI);
    TraverseTypeLoc (TSI->getTypeLoc ());
    ptr<TypeLoc> type = stack.pop ();

    clang::StringRef name = D->getName ();

    stack.push (mkVarDecl (type, name));

    return true;
  }


#define TRANSLATIONUNIT(CLASS, BASE)
  bool TraverseTranslationUnitDecl (clang::TranslationUnitDecl *D)
  {
    TRACE;
    stack.push_mark ();
    Base::TraverseTranslationUnitDecl (D);

    std::vector<ptr<Decl>> decls = stack.pop_marked ();
    stack.push (mkTranslationUnitDecl (decls));

    return true;
  }


#define ABSTRACT_DECL(DECL)
#define DECL(CLASS, BASE)	UNIMP (Decl, CLASS##Decl)
#include <clang/AST/DeclNodes.inc>


  /****************************************************
   * Final result
   */

  ptr<Decl> result ()
  {
    assert (stack.size () == 1);
    return stack.pop ();
  }
};


ptr<Decl>
adt_of_clangAST (clang::TranslationUnitDecl const *D)
{
  OCamlVisitor visitor;
  D->dump ();
  visitor.TraverseDecl (const_cast<clang::TranslationUnitDecl *> (D));
  return visitor.result ();
}
