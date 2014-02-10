#include "OCamlVisitor.h"
#include "dynamic_stack.h"
#include "trace.h"

#include <boost/range/iterator_range.hpp>

using namespace hello_cpp;

#include "enums.h"

struct delayed_exit
{
  int code;
  ~delayed_exit () { exit (code); }
};

#define TODO std::printf ("TODO: %s\n", __func__); delayed_exit E { 1 }

#include "clang_hacks.h"


struct OCamlVisitor
  : clang::RecursiveASTVisitor<OCamlVisitor>
{
private:
  typedef clang::RecursiveASTVisitor<OCamlVisitor> Base;

  dynamic_stack stack;

  /****************************************************
   * {{{1 Traversal dispatch functions
   */


  // Traverse a clang object, but keep it on the stack.
  // Requires that if p is a pointer or TypeLoc, it is not null.
  // If Dense is true, this function checks that the called
  // traversal function pushes exactly one element on the stack
  // (potentially after pushing and popping many others).
  template<bool Dense, typename T, bool (OCamlVisitor::*Fun) (T p)>
  void traverse (T p)
  {
    size_t size_before = stack.size ();
    (this->*Fun) (p);
    size_t size_after = stack.size ();

    if (Dense)
      // Require stack to increase by exactly 1 (one value was pushed).
      assert (size_after == size_before + 1);
    else
      // Require zero or one.
      assert (size_after <= size_before + 1);
  }

  // TODO: unify this with the above
  template<bool Dense, typename T, typename A1, bool (OCamlVisitor::*Fun) (T p, A1 a1)>
  void traverse (T p, A1 a1)
  {
    size_t size_before = stack.size ();
    (this->*Fun) (p, a1);
    size_t size_after = stack.size ();

    if (Dense)
      // Require stack to increase by exactly 1 (one value was pushed).
      assert (size_after == size_before + 1);
    else
      // Require zero or one.
      assert (size_after <= size_before + 1);
  }

  // These functions require the clang pointer to be non-null.
  // In case of TypeLoc, require the contained Type pointer
  // to be non-null.

  // Overloads to call appropriate traversal functions.
  template<bool Dense = true>
  void traverse (clang::Decl *D)
  { assert (D); traverse<Dense, clang::Decl *, &OCamlVisitor::TraverseDecl> (D); }

  template<bool Dense = true>
  void traverse (clang::Stmt *S)
  { assert (S); traverse<Dense, clang::Stmt *, &OCamlVisitor::TraverseStmt> (S); }

  template<bool Dense = true>
  void traverse (clang::TypeLoc TL)
  { assert (TL); traverse<Dense, clang::TypeLoc, &OCamlVisitor::TraverseTypeLoc> (TL); }

  // This is an object without a notion of nullability, so the
  // assert is missing.
  template<bool Dense = true>
  void traverse (clang::DesignatedInitExpr::Designator const &D, clang::DesignatedInitExpr *S)
  { traverse<Dense, clang::DesignatedInitExpr::Designator, clang::DesignatedInitExpr *, &OCamlVisitor::TraverseDesignator> (D, S); }


  // May take a pointer or an object.
  // This function and maybe_traverse both pop and return the
  // translation result (OCaml bridge object).
  template<typename T>
  dynamic_stack::element must_traverse (T p)
  {
    // Traverse, which does not pop.
    traverse<true> (p);
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


  template<bool Dense = true, typename Range, typename... Args>
  dynamic_stack::range traverse_list (Range const &range, Args... args)
  {
    stack.push_mark ();
    for (auto child : range)
      traverse<Dense> (child, args...);
    return stack.pop_marked ();
  }

  // }}}


  /****************************************************
   * {{{1 Common helpers
   */

  // Works for FunctionDecl and DeclRefExpr, who don't share an
  // interface containing getNameInfo (hence the template).
  template<typename DeclT>
  static clang::StringRef getName (DeclT const *D)
  {
    clang::IdentifierInfo *info = D->getNameInfo ().getName ().getAsIdentifierInfo ();
    assert (info);
    return info->getName ();
  }

  // TypedefDecl and DeclaratorDecl
  template<typename DeclT>
  ptr<TypeLoc> getTypeLoc (DeclT const *D)
  {
    clang::TypeSourceInfo *TSI = D->getTypeSourceInfo ();
    assert (TSI);
    return must_traverse (TSI->getTypeLoc ());
  }

  // }}}


public:
  /****************************************************
   * {{{1 Visitor settings
   */

  //bool shouldVisitImplicitCode () const { return true; }

  // }}}

  /****************************************************
   * {{{1 Unary/binary operators
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


#define CONDITIONALOPERATOR(CLASS, BASE)
  bool TraverseConditionalOperator (clang::ConditionalOperator *S)
  {
    TRACE;

    ptr<Expr> cond = must_traverse (S->getCond ());
    ptr<Expr> trueExpr = must_traverse (S->getTrueExpr ());
    ptr<Expr> falseExpr = must_traverse (S->getFalseExpr ());

    stack.push (mkConditionalOperator (cond, trueExpr, falseExpr));

    return true;
  }

  // }}}

  /****************************************************
   * {{{1 Literals
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

  // }}}

  /****************************************************
   * {{{1 Expressions
   */


#define IMPLICITVALUEINITEXPR(CLASS, BASE)
  bool TraverseImplicitValueInitExpr (clang::ImplicitValueInitExpr *S)
  {
    TRACE;

    stack.push (mkImplicitValueInitExpr ());

    return true;
  }


#define ARRAYSUBSCRIPTEXPR(CLASS, BASE)
  bool TraverseArraySubscriptExpr (clang::ArraySubscriptExpr *S)
  {
    TRACE;

    ptr<Expr> base = must_traverse (S->getBase ());
    ptr<Expr> idx = must_traverse (S->getIdx ());

    stack.push (mkArraySubscriptExpr (base, idx));

    return true;
  }


#define STMTEXPR(CLASS, BASE)
  bool TraverseStmtExpr (clang::StmtExpr *S)
  {
    TRACE;

    ptr<Stmt> stmt = must_traverse (S->getSubStmt ());

    stack.push (mkStmtExpr (stmt));

    return true;
  }


#define DECLREFEXPR(CLASS, BASE)
  bool TraverseDeclRefExpr (clang::DeclRefExpr *S)
  {
    TRACE;

    clang::StringRef name = getName (S);

    stack.push (mkDeclRefExpr (name));

    return true;
  }


#define PREDEFINEDEXPR(CLASS, BASE)
  bool TraversePredefinedExpr (clang::PredefinedExpr *S)
  {
    TRACE;

    PredefinedIdent kind = translate_predefined_ident (S->getIdentType ());

    stack.push (mkPredefinedExpr (kind));

    return true;
  }


#define CSTYLECASTEXPR(CLASS, BASE)
  bool TraverseCStyleCastExpr (clang::CStyleCastExpr *S)
  {
    TRACE;

    ptr<TypeLoc> type = must_traverse (S->getTypeInfoAsWritten ()->getTypeLoc ());
    ptr<Expr> sub = must_traverse (S->getSubExpr ());

    stack.push (mkCStyleCastExpr (type, sub));

    return true;
  }


#define IMPLICITCASTEXPR(CLASS, BASE)
  bool TraverseImplicitCastExpr (clang::ImplicitCastExpr *S)
  {
    TRACE;

    ptr<Expr> sub = must_traverse (S->getSubExpr ());

    stack.push (mkImplicitCastExpr (sub));

    return true;
  }


#define PARENEXPR(CLASS, BASE)
  bool TraverseParenExpr (clang::ParenExpr *S)
  {
    TRACE;

    ptr<Expr> sub = must_traverse (S->getSubExpr ());

    stack.push (mkParenExpr (sub));

    return true;
  }


#define COMPOUNDLITERALEXPR(CLASS, BASE)
  bool TraverseCompoundLiteralExpr (clang::CompoundLiteralExpr *S)
  {
    TRACE;

    ptr<TypeLoc> type = getTypeLoc (S);
    ptr<Expr> init = must_traverse (S->getInitializer ());

    stack.push (mkCompoundLiteralExpr (type, init));

    return true;
  }


  bool TraverseDesignator (clang::DesignatedInitExpr::Designator D,
                           clang::DesignatedInitExpr *S)
  {
    TRACE;

    if (D.isFieldDesignator ())
      {
        clang::FieldDecl *field = D.getField ();
        assert (field);
        clang::StringRef name = field->getName ();

        stack.push (mkFieldDesignator (name));
      }
    else if (D.isArrayDesignator ())
      {
        ptr<Expr> index = must_traverse (S->getArrayIndex (D));

        stack.push (mkArrayDesignator (index));
      }
    else if (D.isArrayRangeDesignator ())
      {
        ptr<Expr> start = must_traverse (S->getArrayRangeStart (D));
        ptr<Expr> end   = must_traverse (S->getArrayRangeEnd (D));

        stack.push (mkArrayRangeDesignator (start, end));
      }
    else
      assert (!"Invalid or unknown designator");

    return true;
  }

#define DESIGNATEDINITEXPR(CLASS, BASE)
  bool TraverseDesignatedInitExpr (clang::DesignatedInitExpr *S)
  {
    TRACE;

    list<Designator> designators = traverse_list (designator_range (S), S);
    ptr<Expr> init = must_traverse (S->getInit ());

    stack.push (mkDesignatedInitExpr (designators, init));

    return true;
  }


#define INITLISTEXPR(CLASS, BASE)
  bool TraverseInitListExpr (clang::InitListExpr *S)
  {
    TRACE;

    list<Expr> inits = traverse_list (S->children ());

    stack.push (mkInitListExpr (inits));

    return true;
  }


#define CALLEXPR(CLASS, BASE)
  bool TraverseCallExpr (clang::CallExpr *S)
  {
    TRACE;

    ptr<Expr> callee = must_traverse (S->getCallee ());
    list<Expr> args = traverse_list (arg_range (S));

    stack.push (mkCallExpr (callee, args));

    return true;
  }


#define MEMBEREXPR(CLASS, BASE)
  bool TraverseMemberExpr (clang::MemberExpr *S)
  {
    TRACE;

    ptr<Expr> base = must_traverse (S->getBase ());
    clang::StringRef member = S->getMemberDecl ()->getName ();
    bool isArrow = S->isArrow ();

    stack.push (mkMemberExpr (base, member, isArrow));

    return true;
  }


  template<typename TypeFactory, typename ExprFactory>
  void pushUnaryExprOrTypeTraitExpr (clang::UnaryExprOrTypeTraitExpr *S,
                                     TypeFactory mkOfType, ExprFactory mkOfExpr)
  {
    ptr<Expr> expr;
    if (S->isArgumentType ())
      expr = mkSizeOfType (must_traverse (S->getArgumentTypeInfo ()->getTypeLoc ()));
    else
      expr = mkSizeOfExpr (must_traverse (S->getArgumentExpr ()));
    stack.push (expr);
  }


#define UNARYEXPRORTYPETRAITEXPR(CLASS, BASE)
  bool TraverseUnaryExprOrTypeTraitExpr (clang::UnaryExprOrTypeTraitExpr *S)
  {
    TRACE;

    switch (S->getKind ())
      {
      case clang::UETT_SizeOf:
        pushUnaryExprOrTypeTraitExpr (S, mkSizeOfType, mkSizeOfExpr);
        break;
      case clang::UETT_AlignOf:
        pushUnaryExprOrTypeTraitExpr (S, mkAlignOfType, mkAlignOfExpr);
        break;
      case clang::UETT_VecStep:
        pushUnaryExprOrTypeTraitExpr (S, mkVecStepType, mkVecStepExpr);
        break;
      }

    return true;
  }

  // }}}

  /****************************************************
   * {{{1 Statements
   */

  // Simple forwards so passing it as template argument in `traverse' works.
  bool TraverseDecl (clang::Decl *D) { return Base::TraverseDecl (D); }
  bool TraverseStmt (clang::Stmt *S) { return Base::TraverseStmt (S); }

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


#define LABELSTMT(CLASS, BASE)
  bool TraverseLabelStmt (clang::LabelStmt *S)
  {
    TRACE;

    clang::StringRef name = S->getDecl ()->getName ();
    ptr<Stmt> sub = must_traverse (S->getSubStmt ());

    stack.push (mkLabelStmt (name, sub));

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


#define DEFAULTSTMT(CLASS, BASE)
  bool TraverseDefaultStmt (clang::DefaultStmt *S)
  {
    TRACE;

    ptr<Stmt> sub = must_traverse (S->getSubStmt ());

    stack.push (mkDefaultStmt (sub));

    return true;
  }


#define GOTOSTMT(CLASS, BASE)
  bool TraverseGotoStmt (clang::GotoStmt *S)
  {
    TRACE;

    clang::StringRef name = S->getLabel ()->getName ();

    stack.push (mkGotoStmt (name));

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


#define WHILESTMT(CLASS, BASE)
  bool TraverseWhileStmt (clang::WhileStmt *S)
  {
    TRACE;

    ptr<Expr> cond = maybe_traverse (S->getCond ());
    ptr<Stmt> body = must_traverse (S->getBody ());

    stack.push (mkWhileStmt (cond, body));

    return true;
  }


#define DOSTMT(CLASS, BASE)
  bool TraverseDoStmt (clang::DoStmt *S)
  {
    TRACE;

    ptr<Stmt> body = must_traverse (S->getBody ());
    ptr<Expr> cond = maybe_traverse (S->getCond ());

    stack.push (mkDoStmt (body, cond));

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
    list<Stmt> stmts = stack.pop_marked ();

    stack.push (mkCompoundStmt (stmts));

    return true;
  }


#define GCCASMSTMT(CLASS, BASE)
  bool TraverseGCCAsmStmt (clang::GCCAsmStmt *S)
  {
    TRACE;

    // TODO: implement
    stack.push (mkUnimpStmt ("GCCAsmStmt"));

    return true;
  }


#define DECLSTMT(CLASS, BASE)
  bool TraverseDeclStmt (clang::DeclStmt *S)
  {
    TRACE;

    list<Decl> decls = traverse_list (decl_range (S));

    stack.push (mkDeclStmt (decls));

    return true;
  }


#define IGNORE_ADT(CLASS, VAR)					\
    stack.push_mark ();						\
    Base::Traverse##CLASS (VAR);				\
    /* Drop everything made by previous calls. */		\
    size_t marker = stack.pop_mark ();				\
    while (marker--) stack.pop ()


#define UNIMP(TYPE, CLASS)					\
  bool Traverse##CLASS (clang::CLASS *Node)			\
  {								\
    TODO;							\
    TRACE;							\
    IGNORE_ADT (CLASS, Node);					\
    stack.push (mkUnimp##TYPE (#CLASS));			\
    return true;						\
  }


#define UNIMP_INTF(TYPE, CLASS)					\
  bool Traverse##CLASS (clang::CLASS *Node);

#define UNIMP_IMPL(TYPE, CLASS)					\
  bool OCamlVisitor::Traverse##CLASS (clang::CLASS *Node)	\
  {								\
    TODO;							\
    TRACE;							\
    IGNORE_ADT (CLASS, Node);					\
    stack.push (mkUnimp##TYPE (#CLASS));			\
    return true;						\
  }


#define ABSTRACT_STMT(STMT)
#define STMT(CLASS, BASE)	UNIMP (Stmt, CLASS)
#define EXPR(CLASS, BASE)	UNIMP (Expr, CLASS)
#include <clang/AST/StmtNodes.inc>

  // }}}

  /****************************************************
   * {{{1 Types
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

  // }}}

  /****************************************************
   * {{{1 TypeLocs
   */

  bool TraverseTypeLoc (clang::TypeLoc TL);

#define ABSTRACT_TYPELOC(CLASS, BASE)
#define TYPELOC(CLASS, BASE)					\
  bool Traverse##CLASS##TypeLoc (clang::CLASS##TypeLoc TL);
#include <clang/AST/TypeLocNodes.def>

  // }}}

  /****************************************************
   * {{{1 Declarations
   */

#define FUNCTION(CLASS, BASE)
  bool TraverseFunctionDecl (clang::FunctionDecl *D);

#define EMPTY(CLASS, BASE)
  bool TraverseEmptyDecl (clang::EmptyDecl *D);

#define TYPEDEF(CLASS, BASE)
  bool TraverseTypedefDecl (clang::TypedefDecl *D);

#define RECORD(CLASS, BASE)
  bool TraverseRecordDecl (clang::RecordDecl *D);

#define FIELD(CLASS, BASE)
  bool TraverseFieldDecl (clang::FieldDecl *D);

#define ENUM(CLASS, BASE)
  bool TraverseEnumDecl (clang::EnumDecl *D);

#define ENUMCONSTANT(CLASS, BASE)
  bool TraverseEnumConstantDecl (clang::EnumConstantDecl *D);

#define PARMVAR(CLASS, BASE)
  bool TraverseParmVarDecl (clang::ParmVarDecl *D);

#define VAR(CLASS, BASE)
  bool TraverseVarDecl (clang::VarDecl *D);

#define TRANSLATIONUNIT(CLASS, BASE)
  bool TraverseTranslationUnitDecl (clang::TranslationUnitDecl *D);

#define ABSTRACT_DECL(DECL)
#define DECL(CLASS, BASE)	UNIMP_INTF (Decl, CLASS##Decl)
#include <clang/AST/DeclNodes.inc>

  // }}}

  /****************************************************
   * Final result
   */

  ptr<Decl> translate (clang::TranslationUnitDecl const *D)
  {
    assert (stack.empty ());
    return must_traverse (const_cast<clang::TranslationUnitDecl *> (D));
  }
};


/****************************************************
 * {{{1 TypeLocs
 */

bool
OCamlVisitor::TraverseTypeLoc (clang::TypeLoc TL)
{
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

  // Validate that a TypeLoc was actually created, not some other
  // kind of node (such as Expr).
  ptr<TypeLoc> type = stack.pop ();
  stack.push (type);

  return true;
}


bool
OCamlVisitor::TraverseBuiltinTypeLoc (clang::BuiltinTypeLoc TL)
{
  TRACE;

  BuiltinType bt = translate_builtin_type (TL.getTypePtr ()->getKind ());

  stack.push (mkBuiltinTypeLoc (bt));

  return true;
}


bool
OCamlVisitor::TraverseTypeOfExprTypeLoc (clang::TypeOfExprTypeLoc TL)
{
  TRACE;

  ptr<Expr> expr = must_traverse (TL.getUnderlyingExpr ());

  stack.push (mkTypeOfExprTypeLoc (expr));

  return true;
}


bool
OCamlVisitor::TraverseTypeOfTypeLoc (clang::TypeOfTypeLoc TL)
{
  TRACE;

  ptr<TypeLoc> type = must_traverse (TL.getUnderlyingTInfo ()->getTypeLoc ());

  stack.push (mkTypeOfTypeLoc (type));

  return true;
}


bool
OCamlVisitor::TraverseConstantArrayTypeLoc (clang::ConstantArrayTypeLoc TL)
{
  TRACE;

  ptr<TypeLoc> element = must_traverse (TL.getElementLoc ());
  uint64_t size = TL.getTypePtr ()->getSize ().getZExtValue ();

  stack.push (mkConstantArrayTypeLoc (element, size));

  return true;
}


bool
OCamlVisitor::TraverseVariableArrayTypeLoc (clang::VariableArrayTypeLoc TL)
{
  TRACE;

  ptr<TypeLoc> element = must_traverse (TL.getElementLoc ());
  ptr<Expr> size = must_traverse (TL.getSizeExpr ());

  stack.push (mkVariableArrayTypeLoc (element, size));

  return true;
}


bool
OCamlVisitor::TraverseIncompleteArrayTypeLoc (clang::IncompleteArrayTypeLoc TL)
{
  TRACE;

  ptr<TypeLoc> element = must_traverse (TL.getElementLoc ());

  stack.push (mkIncompleteArrayTypeLoc (element));

  return true;
}


bool
OCamlVisitor::TraversePointerTypeLoc (clang::PointerTypeLoc TL)
{
  TRACE;

  ptr<TypeLoc> pointee = must_traverse (TL.getPointeeLoc ());

  stack.push (mkPointerTypeLoc (pointee));

  return true;
}


bool
OCamlVisitor::TraverseElaboratedTypeLoc (clang::ElaboratedTypeLoc TL)
{
  TRACE;

  TraverseNestedNameSpecifierLoc (TL.getQualifierLoc ());
  ptr<TypeLoc> type = must_traverse (TL.getNamedTypeLoc ());

  stack.push (mkElaboratedTypeLoc (type));

  return true;
}


//bool
//OCamlVisitor::TraverseQualifiedTypeLoc (clang::QualifiedTypeLoc TL)
//{
  //TRACE;

  //clang::StringRef name = TL.getDecl ()->getName ();

  //stack.push (mkQualifiedTypeLoc (name));

  //return true;
//}


bool
OCamlVisitor::TraverseEnumTypeLoc (clang::EnumTypeLoc TL)
{
  TRACE;

  clang::StringRef name = TL.getDecl ()->getName ();

  stack.push (mkEnumTypeLoc (name));

  return true;
}


bool
OCamlVisitor::TraverseRecordTypeLoc (clang::RecordTypeLoc TL)
{
  TRACE;

  TagTypeKind kind = translate_tag_type_kind (TL.getDecl ()->getTagKind ());
  clang::StringRef name = TL.getDecl ()->getName ();

  stack.push (mkRecordTypeLoc (kind, name));

  return true;
}


bool
OCamlVisitor::TraverseFunctionNoProtoTypeLoc (clang::FunctionNoProtoTypeLoc TL)
{
  TRACE;

  ptr<TypeLoc> result = must_traverse (TL.getResultLoc ());

  stack.push (mkFunctionNoProtoTypeLoc (result));

  return true;
}


bool
OCamlVisitor::TraverseFunctionProtoTypeLoc (clang::FunctionProtoTypeLoc TL)
{
  TRACE;

  ptr<TypeLoc> result = must_traverse (TL.getResultLoc ());
  list<Decl> args = traverse_list (TL.getParams ());

  // TODO: exceptions

  stack.push (mkFunctionProtoTypeLoc (result, args));

  return true;
}


bool
OCamlVisitor::TraverseTypedefTypeLoc (clang::TypedefTypeLoc TL)
{
  TRACE;

  clang::StringRef name = TL.getTypedefNameDecl ()->getName ();
  stack.push (mkTypedefTypeLoc (name));

  return true;
}


#define UNIMP_TYPE(CLASS)						\
bool									\
OCamlVisitor::Traverse##CLASS##TypeLoc (clang::CLASS##TypeLoc TL)	\
{									\
  TRACE;								\
  stack.push (mkUnimpTypeLoc (#CLASS));					\
  return true;								\
}

UNIMP_TYPE (Atomic)
UNIMP_TYPE (Attributed)
UNIMP_TYPE (Auto)
UNIMP_TYPE (BlockPointer)
UNIMP_TYPE (Complex)
UNIMP_TYPE (Decayed)
UNIMP_TYPE (Decltype)
UNIMP_TYPE (DependentName)
UNIMP_TYPE (DependentSizedArray)
UNIMP_TYPE (DependentSizedExtVector)
UNIMP_TYPE (DependentTemplateSpecialization)
UNIMP_TYPE (ExtVector)
UNIMP_TYPE (InjectedClassName)
UNIMP_TYPE (LValueReference)
UNIMP_TYPE (MemberPointer)
UNIMP_TYPE (ObjCInterface)
UNIMP_TYPE (ObjCObject)
UNIMP_TYPE (ObjCObjectPointer)
UNIMP_TYPE (PackExpansion)
UNIMP_TYPE (Paren)
UNIMP_TYPE (Qualified)
UNIMP_TYPE (RValueReference)
UNIMP_TYPE (SubstTemplateTypeParm)
UNIMP_TYPE (SubstTemplateTypeParmPack)
UNIMP_TYPE (TemplateSpecialization)
UNIMP_TYPE (TemplateTypeParm)
UNIMP_TYPE (UnaryTransform)
UNIMP_TYPE (UnresolvedUsing)
UNIMP_TYPE (Vector)

// }}}

/****************************************************
 * {{{1 Declarations
 */

#define FUNCTION(CLASS, BASE)
bool
OCamlVisitor::TraverseFunctionDecl (clang::FunctionDecl *D)
{
  TRACE;

  // TODO: what are these? probably irrelevant in C.
  TraverseNestedNameSpecifierLoc (D->getQualifierLoc ());
  TraverseDeclarationNameInfo (D->getNameInfo ());

  // Function type, including parameters.
  ptr<TypeLoc> type;
  if (D->getTypeSourceInfo ())
    type = getTypeLoc (D);
  else
    {
      // TODO: implement this

      // Some special functions such as printf don't have a valid TSI.
      ptr<TypeLoc> result = mkBuiltinTypeLoc (BT_Void);
      list<Decl> args;// = traverse_list (param_range (D));

      // TODO: exceptions

      type = mkFunctionProtoTypeLoc (result, args);

      throw std::runtime_error ("unsupported built-in function declaration");
    }

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

#define EMPTY(CLASS, BASE)
bool
OCamlVisitor::TraverseEmptyDecl (clang::EmptyDecl *D)
{
  TRACE;

  stack.push (mkEmptyDecl ());

  return true;
}


#define TYPEDEF(CLASS, BASE)
bool
OCamlVisitor::TraverseTypedefDecl (clang::TypedefDecl *D)
{
  TRACE;

  ptr<TypeLoc> type = getTypeLoc (D);
  clang::StringRef name = D->getName ();

  stack.push (mkTypedefDecl (type, name));

  return true;
}


#define RECORD(CLASS, BASE)
bool
OCamlVisitor::TraverseRecordDecl (clang::RecordDecl *D)
{
  TRACE;

  list<Decl> members = traverse_list (decl_range (D));
  clang::StringRef name = D->getName ();

  stack.push (mkRecordDecl (name, members));

  return true;
}


#define FIELD(CLASS, BASE)
bool
OCamlVisitor::TraverseFieldDecl (clang::FieldDecl *D)
{
  TRACE;

  ptr<TypeLoc> type = getTypeLoc (D);

  option<Expr> bitwidth;
  if (D->isBitField ())
    bitwidth = must_traverse (D->getBitWidth ());

  option<Expr> init;
  if (D->hasInClassInitializer ())
    init = must_traverse (D->getInClassInitializer ());

  clang::StringRef name = D->getName ();

  stack.push (mkFieldDecl (type, name, bitwidth, init));

  return true;
}


#define ENUM(CLASS, BASE)
bool
OCamlVisitor::TraverseEnumDecl (clang::EnumDecl *D)
{
  TRACE;

  clang::StringRef name = D->getName ();
  list<Decl> enumerators = traverse_list (decl_range (D));

  stack.push (mkEnumDecl (name, enumerators));

  return true;
}


#define ENUMCONSTANT(CLASS, BASE)
bool
OCamlVisitor::TraverseEnumConstantDecl (clang::EnumConstantDecl *D)
{
  TRACE;

  clang::StringRef name = D->getName ();
  option<Expr> init = maybe_traverse (D->getInitExpr ());

  stack.push (mkEnumConstantDecl (name, init));

  return true;
}


#define PARMVAR(CLASS, BASE)
bool
OCamlVisitor::TraverseParmVarDecl (clang::ParmVarDecl *D)
{
  TRACE;

  TraverseNestedNameSpecifierLoc (D->getQualifierLoc ());

  ptr<TypeLoc> type = getTypeLoc (D);
  clang::StringRef name = D->getName ();

  stack.push (mkParmVarDecl (type, name));

  return true;
}


#define VAR(CLASS, BASE)
bool
OCamlVisitor::TraverseVarDecl (clang::VarDecl *D)
{
  TRACE;

  TraverseNestedNameSpecifierLoc (D->getQualifierLoc ());

  ptr<TypeLoc> type = getTypeLoc (D);
  clang::StringRef name = D->getName ();
  option<Expr> init = maybe_traverse (D->getInit ());

  stack.push (mkVarDecl (type, name, init));

  return true;
}


#define TRANSLATIONUNIT(CLASS, BASE)
bool
OCamlVisitor::TraverseTranslationUnitDecl (clang::TranslationUnitDecl *D)
{
  TRACE;

  // Dense is false for this call, because TranslationUnitDecls
  // may contain implicit declarations, which we skip here.
  list<Decl> decls = traverse_list<false> (decl_range (D));

  stack.push (mkTranslationUnitDecl (decls));

  return true;
}

UNIMP_IMPL (Decl, AccessSpecDecl)
UNIMP_IMPL (Decl, BlockDecl)
UNIMP_IMPL (Decl, CapturedDecl)
UNIMP_IMPL (Decl, ClassScopeFunctionSpecializationDecl)
UNIMP_IMPL (Decl, ClassTemplateDecl)
UNIMP_IMPL (Decl, FileScopeAsmDecl)
UNIMP_IMPL (Decl, FriendDecl)
UNIMP_IMPL (Decl, FriendTemplateDecl)
UNIMP_IMPL (Decl, FunctionTemplateDecl)
UNIMP_IMPL (Decl, ImportDecl)
UNIMP_IMPL (Decl, IndirectFieldDecl)
UNIMP_IMPL (Decl, LabelDecl)
UNIMP_IMPL (Decl, LinkageSpecDecl)
UNIMP_IMPL (Decl, MSPropertyDecl)
UNIMP_IMPL (Decl, NamespaceAliasDecl)
UNIMP_IMPL (Decl, NamespaceDecl)
UNIMP_IMPL (Decl, NonTypeTemplateParmDecl)
UNIMP_IMPL (Decl, ObjCCategoryDecl)
UNIMP_IMPL (Decl, ObjCCategoryImplDecl)
UNIMP_IMPL (Decl, ObjCCompatibleAliasDecl)
UNIMP_IMPL (Decl, ObjCImplementationDecl)
UNIMP_IMPL (Decl, ObjCInterfaceDecl)
UNIMP_IMPL (Decl, ObjCMethodDecl)
UNIMP_IMPL (Decl, ObjCPropertyDecl)
UNIMP_IMPL (Decl, ObjCPropertyImplDecl)
UNIMP_IMPL (Decl, ObjCProtocolDecl)
UNIMP_IMPL (Decl, OMPThreadPrivateDecl)
UNIMP_IMPL (Decl, StaticAssertDecl)
UNIMP_IMPL (Decl, TemplateTemplateParmDecl)
UNIMP_IMPL (Decl, TemplateTypeParmDecl)
UNIMP_IMPL (Decl, TypeAliasDecl)
UNIMP_IMPL (Decl, TypeAliasTemplateDecl)
UNIMP_IMPL (Decl, UnresolvedUsingTypenameDecl)
UNIMP_IMPL (Decl, UnresolvedUsingValueDecl)
UNIMP_IMPL (Decl, UsingDecl)
UNIMP_IMPL (Decl, UsingDirectiveDecl)
UNIMP_IMPL (Decl, UsingShadowDecl)
UNIMP_IMPL (Decl, VarTemplateDecl)

// }}}


ptr<Decl>
adt_of_clangAST (clang::TranslationUnitDecl const *D)
{
  OCamlVisitor visitor;
  //D->dump ();
  return visitor.translate (D);
}
