#include "OCamlVisitor.h"
#include "dynamic_stack.h"
#include "trace.h"

#include <clang/Basic/SourceManager.h>

#include <boost/range/iterator_range.hpp>
#include <boost/range/adaptor/filtered.hpp>

#include <map>
#include <sstream>

using namespace bridge_ast;

#include "enums.h"

struct delayed_exit
{
  int code;
  ~delayed_exit () { exit (code); }
};

#define TODO std::printf ("TODO: %s\n", __func__); delayed_exit E { 1 }

#include "clang_hacks.h"

#undef TRACE
#define TRACE

template<typename T>
struct clang_compare
{
  typedef T type;
  bool operator () (T a, T b) { return a < b; }
};


static bool
operator < (clang::Qualifiers a, clang::Qualifiers b)
{
  return a == b ? 0 : b.isStrictSupersetOf (a);
}

template<>
bool
clang_compare<clang::QualType>::operator () (type a, type b)
{
  return a.getTypePtr () < b.getTypePtr ()
      || a.getLocalQualifiers () < b.getLocalQualifiers ();
}


template<>
bool
clang_compare<clang::TypeLoc>::operator () (type a, type b)
{
  return clang_compare<clang::QualType> () (a.getType (), b.getType ());
}


struct OCamlVisitor
  : clang::RecursiveASTVisitor<OCamlVisitor>
{
  // Enable caching (and sharing) of bridge AST objects.
  bool sharing = true;
  // Enable sharing for Type (and Ctyp) objects.
  bool share_types = true;
  // Also enable sharing for TypeLoc objects, ignoring source
  // locations, thus making source locations for TypeLocs useless.
  bool share_type_locs = false;

  // Resolve all source locations.
  bool with_sloc = true;

  // Not static, because creating bridge objects has side effects.
  ptr<Sloc> const empty_sloc = mkSloc ();

private:
  typedef clang::RecursiveASTVisitor<OCamlVisitor> Base;

  clang::SourceManager &SM;
  dynamic_stack stack;

  /****************************************************
   * {{{1 Traversal dispatch functions
   */

  template<typename T>
  adt_ptr cached_value (T p, adt_ptr value)
  {
    if (!sharing)
      return nullptr;

    static std::map<T, adt_ptr, clang_compare<T>> cache;
    if (value)
      {
        cache.insert (std::make_pair (p, value));
        return value;
      }
    else
      {
        auto found = cache.find (p);
        if (found == cache.end ())
          return nullptr;
        return found->second;
      }
  }

  adt_ptr cached (clang::QualType p, adt_ptr value = nullptr)
  { return share_types ? cached_value (p, value) : nullptr; }

  adt_ptr cached (clang::TypeLoc p, adt_ptr value = nullptr)
  { return share_type_locs ? cached_value (p, value) : nullptr; }

  adt_ptr cached (clang::DesignatedInitExpr::Designator p, adt_ptr value = nullptr)
  { return nullptr; }

  adt_ptr cached (clang::Decl *p, adt_ptr value = nullptr)
  { return nullptr; }

  adt_ptr cached (clang::Stmt *p, adt_ptr value = nullptr)
  { return nullptr; }


  template<typename T>
  static void dump (T p)
  { p->dump (); }

  static void dump (clang::TypeLoc TL)
  { dump (TL.getType ()); }

  static void dump (clang::DesignatedInitExpr::Designator p)
  { puts ("<clang::DesignatedInitExpr::Designator>"); }


  template<typename T>
  static void check_size (T p, size_t size_before, size_t size_after)
  {
    // Require stack to increase by exactly 1 (one value was pushed).
    if (size_after != size_before + 1)
      {
        dump (p);
        std::stringstream message;
        message << "Traversal function postcondition not held: "
                << "must create exactly 1 value, but created "
                << size_after - size_before;
        throw std::runtime_error (message.str ());
      }
  }

  // Traverse a clang object, but keep the result (if any) on
  // the stack. Requires that if p is a pointer or TypeLoc, it
  // is not null.
  template<typename T, bool (OCamlVisitor::*Fun) (T p)>
  void traverse (T p)
  {
    adt_ptr result = cached (p);
    if (result)
      stack.push (result);
    else
      {
        size_t size_before = stack.size ();
        (this->*Fun) (p);
        size_t size_after = stack.size ();

        check_size (p, size_before, size_after);

        cached (p, stack.top ());
      }
  }

  // TODO: unify this with the above
  template<typename T, typename A1, bool (OCamlVisitor::*Fun) (T p, A1 a1)>
  void traverse (T p, A1 a1)
  {
    adt_ptr result = cached (p);
    if (result)
      stack.push (result);
    else
      {
        size_t size_before = stack.size ();
        (this->*Fun) (p, a1);
        size_t size_after = stack.size ();

        check_size (p, size_before, size_after);

        cached (p, stack.top ());
      }
  }

  // These functions require the clang pointer to be non-null.
  // In case of TypeLoc, require the contained Type pointer
  // to be non-null.

  // Overloads to call appropriate traversal functions.
  void traverse (clang::Decl *D)
  { assert (D); traverse<clang::Decl *, &OCamlVisitor::TraverseDecl> (D); }

  void traverse (clang::Stmt *S)
  { assert (S); traverse<clang::Stmt *, &OCamlVisitor::TraverseStmt> (S); }

  void traverse (clang::TypeLoc TL)
  { assert (TL); traverse<clang::TypeLoc, &OCamlVisitor::TraverseTypeLoc> (TL); }

  void traverse (clang::QualType T)
  { assert (!T.isNull ()); traverse<clang::QualType, &OCamlVisitor::TraverseType> (T); }

  // This is an object without a notion of nullability, so the
  // assert is missing.
  void traverse (clang::DesignatedInitExpr::Designator const &D, clang::DesignatedInitExpr *S)
  { traverse<clang::DesignatedInitExpr::Designator, clang::DesignatedInitExpr *, &OCamlVisitor::TraverseDesignator> (D, S); }


  // May take a pointer or an object.
  // This function and maybe_traverse both pop and return the
  // translation result (OCaml bridge object).
  template<typename T>
  dynamic_stack::element must_traverse (T p)
  {
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


  template<typename Range, typename... Args>
  dynamic_stack::range traverse_list (Range const &range, Args... args)
  {
    stack.push_mark ();
    for (auto child : range)
      traverse (child, args...);
    return stack.pop_marked ();
  }

  template<typename DeclT>
  dynamic_stack::range traverse_explicit_decls (DeclT *D)
  {
    using namespace boost::adaptors;

    return traverse_list (decl_range (D)
      | filtered ([] (clang::Decl *D) {
          return !D->isImplicit ();
        }));
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

  template<typename T>
  T const &deref (T *p) { return *p; }

  template<typename T>
  T const &deref (T const &p) { return p; }

  template<typename T>
  ptr<Sloc> sloc (T p)
  {
    if (!with_sloc)
      return empty_sloc;

    clang::PresumedLoc start = SM.getPresumedLoc (deref (p).getLocStart ());
    clang::PresumedLoc end   = SM.getPresumedLoc (deref (p).getLocEnd   ());

    assert (start.isValid () == end.isValid ());

    if (start.isInvalid ())
      return empty_sloc;

    ptr<Sloc> sloc = mkSloc ();

    sloc->loc_s_filename = start.getFilename ();
    sloc->loc_s_line     = start.getLine     ();
    sloc->loc_s_column   = start.getColumn   ();
    sloc->loc_e_filename = end  .getFilename ();
    sloc->loc_e_line     = end  .getLine     ();
    sloc->loc_e_column   = end  .getColumn   ();

    return sloc;
  }

  // }}}


public:
  OCamlVisitor (clang::SourceManager &SM)
    : SM (SM)
  {
  }

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
  bool TraverseIntegerLiteral (clang::IntegerLiteral *S)
  {
    stack.push (mkIntegerLiteral
                (S->getValue ().getSExtValue ()));

    return true;
  }


#define CHARACTERLITERAL(CLASS, BASE)
  bool TraverseCharacterLiteral (clang::CharacterLiteral *S)
  {
    stack.push (mkCharacterLiteral
                (S->getValue ()));

    return true;
  }


#define FLOATINGLITERAL(CLASS, BASE)
  bool TraverseFloatingLiteral (clang::FloatingLiteral *S)
  {
    stack.push (mkFloatingLiteral
                (S->getValue ().convertToDouble ()));

    return true;
  }


#define STRINGLITERAL(CLASS, BASE)
  bool TraverseStringLiteral (clang::StringLiteral *S)
  {
    stack.push (mkStringLiteral
                (S->getString ()));

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

    PredefinedExpr kind = translate_predefined_expr (S->getIdentType ());

    stack.push (mkPredefinedExpr (kind));

    return true;
  }


#define CSTYLECASTEXPR(CLASS, BASE)
  bool TraverseCStyleCastExpr (clang::CStyleCastExpr *S)
  {
    TRACE;

    CastKind kind = translate_cast_kind (S->getCastKind ());
    ptr<TypeLoc> type = must_traverse (S->getTypeInfoAsWritten ()->getTypeLoc ());
    ptr<Expr> sub = must_traverse (S->getSubExpr ());

    stack.push (mkCStyleCastExpr (kind, type, sub));

    return true;
  }


#define IMPLICITCASTEXPR(CLASS, BASE)
  bool TraverseImplicitCastExpr (clang::ImplicitCastExpr *S)
  {
    TRACE;

    CastKind kind = translate_cast_kind (S->getCastKind ());
    ptr<Expr> sub = must_traverse (S->getSubExpr ());

    stack.push (mkImplicitCastExpr (kind, sub));

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

    ptr<Designator_> dr;

    if (D.isFieldDesignator ())
      {
        clang::FieldDecl *field = D.getField ();
        assert (field);
        clang::StringRef name = field->getName ();

        dr = mkFieldDesignator (name);
      }
    else if (D.isArrayDesignator ())
      {
        ptr<Expr> index = must_traverse (S->getArrayIndex (D));

        dr = mkArrayDesignator (index);
      }
    else if (D.isArrayRangeDesignator ())
      {
        ptr<Expr> start = must_traverse (S->getArrayRangeStart (D));
        ptr<Expr> end   = must_traverse (S->getArrayRangeEnd (D));

        dr = mkArrayRangeDesignator (start, end);
      }
    else
      assert (!"Invalid or unknown designator");

    ptr<Designator> designator = mkDesignator ();
    designator->dr = dr;
    designator->dr_sloc = sloc (S);
    stack.push (designator);

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


#define VAARGEXPR(CLASS, BASE)
  bool TraverseVAArgExpr (clang::VAArgExpr *S)
  {
    TRACE;

    ptr<Expr> sub = must_traverse (S->getSubExpr ());
    ptr<TypeLoc> type = must_traverse (S->getWrittenTypeInfo ()->getTypeLoc ());

    stack.push (mkVAArgExpr (sub, type));

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
    ptr<Expr_> expr;
    if (S->isArgumentType ())
      expr = mkOfType (must_traverse (S->getArgumentTypeInfo ()->getTypeLoc ()));
    else
      expr = mkOfExpr (must_traverse (S->getArgumentExpr ()));
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

  bool TraverseStmt (clang::Stmt *S)
  {
    Base::TraverseStmt (S);

    if (clang::Expr *E = clang::dyn_cast<clang::Expr> (S))
      {
        ptr<Expr> expr = mkExpr ();
        expr->e = stack.pop ();
        expr->e_sloc = sloc (E);
        expr->e_type = must_traverse (E->getType ());
        stack.push (expr);
      }
    else
      {
        ptr<Stmt> stmt = mkStmt ();
        stmt->s = stack.pop ();
        stmt->s_sloc = sloc (S);
        stack.push (stmt);
      }

    return true;
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
    stack.push (bridge_ast::mkUnimpStmt ("GCCAsmStmt"));

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
  bool Traverse##CLASS (clang::CLASS *S)			\
  {								\
    TODO;							\
    TRACE;							\
    IGNORE_ADT (CLASS, S);					\
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

  bool TraverseType (clang::QualType T);

#define ABSTRACT_TYPE(CLASS, BASE)
#define TYPE(CLASS, BASE)					\
  bool Traverse##CLASS##Type (clang::CLASS##Type *T);
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

  bool TraverseDecl (clang::Decl *D);

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
#define DECL(CLASS, BASE)					\
  bool Traverse##CLASS##Decl (clang::CLASS##Decl *D);
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
 * {{{1 Types
 */


bool
OCamlVisitor::TraverseType (clang::QualType T)
{
  TRACE;

  Base::TraverseType (T);
  ptr<Ctyp_> unqual = stack.pop ();

  clang::Qualifiers quals = T.getLocalQualifiers ();

  std::vector<Qualifier> qualifiers;
  if (quals.hasConst ())	qualifiers.push_back (TQ_Const);
  if (quals.hasVolatile ())	qualifiers.push_back (TQ_Volatile);
  if (quals.hasRestrict ())	qualifiers.push_back (TQ_Restrict);

  switch (quals.getObjCGCAttr ())
    {
    case clang::Qualifiers::GCNone:
      // Nothing to add.
      break;
    case clang::Qualifiers::Weak:
      qualifiers.push_back (TQ_Weak);
      break;
    case clang::Qualifiers::Strong:
      qualifiers.push_back (TQ_Strong);
      break;
    }

  switch (quals.getObjCLifetime ())
    {
    case clang::Qualifiers::OCL_None:
      // Nothing to add.
      break;
    case clang::Qualifiers::OCL_ExplicitNone:
      qualifiers.push_back (TQ_OCL_ExplicitNone);
      break;
    case clang::Qualifiers::OCL_Strong:
      qualifiers.push_back (TQ_OCL_Strong);
      break;
    case clang::Qualifiers::OCL_Weak:
      qualifiers.push_back (TQ_OCL_Weak);
      break;
    case clang::Qualifiers::OCL_Autoreleasing:
      qualifiers.push_back (TQ_OCL_Autoreleasing);
      break;
    }

  option<int> addressSpace;
  if (quals.hasAddressSpace ())
    addressSpace = quals.getAddressSpace ();

  ptr<Ctyp> ctyp = mkCtyp ();
  ctyp->t = unqual;
  ctyp->t_qual = qualifiers;
  ctyp->t_aspace = addressSpace;
  stack.push (ctyp);

  return true;
}


bool
OCamlVisitor::TraverseBuiltinType (clang::BuiltinType *T)
{
  TRACE;

  BuiltinType bt = translate_builtin_type (T->getKind ());

  stack.push (mkBuiltinType (bt));

  return true;
}



bool
OCamlVisitor::TraversePointerType (clang::PointerType *T)
{
  TRACE;

  ptr<Ctyp> pointee = must_traverse (T->getPointeeType ());

  stack.push (mkPointerType (pointee));

  return true;
}


bool
OCamlVisitor::TraverseFunctionProtoType (clang::FunctionProtoType *T)
{
  TRACE;

  ptr<Ctyp> result = must_traverse (T->getResultType ());
  list<Ctyp> args = traverse_list (arg_type_range (T));

  // TODO: exceptions

  stack.push (mkFunctionProtoType (result, args));

  return true;
}


bool
OCamlVisitor::TraverseFunctionNoProtoType (clang::FunctionNoProtoType *T)
{
  TRACE;

  ptr<Ctyp> result = must_traverse (T->getResultType ());

  stack.push (mkFunctionNoProtoType (result));

  return true;
}




#define UNIMP_TYPE(CLASS)					\
bool								\
OCamlVisitor::Traverse##CLASS##Type (clang::CLASS##Type *type)	\
{								\
  TODO;								\
  Base::Traverse##CLASS##Type (type);				\
  return true;							\
}

UNIMP_TYPE(Atomic)
UNIMP_TYPE(Attributed)
UNIMP_TYPE(Auto)
UNIMP_TYPE(BlockPointer)
UNIMP_TYPE(Complex)
bool
OCamlVisitor::TraverseConstantArrayType (clang::ConstantArrayType *T)
{
  TRACE;

  ptr<Ctyp> element = must_traverse (T->getElementType ());
  uint64_t size = T->getSize ().getZExtValue ();

  stack.push (mkConstantArrayType (element, size));

  return true;
}


bool
OCamlVisitor::TraverseDecayedType (clang::DecayedType *T)
{
  TRACE;

  ptr<Ctyp> decayed = must_traverse (T->getDecayedType ());
  ptr<Ctyp> original = must_traverse (T->getOriginalType ());

  stack.push (mkDecayedType (decayed, original));

  return true;
}


UNIMP_TYPE(Decltype)
UNIMP_TYPE(DependentName)
UNIMP_TYPE(DependentSizedArray)
UNIMP_TYPE(DependentSizedExtVector)
UNIMP_TYPE(DependentTemplateSpecialization)
bool
OCamlVisitor::TraverseElaboratedType (clang::ElaboratedType *T)
{
  TRACE;

  TraverseNestedNameSpecifier (T->getQualifier ());
  ptr<Ctyp> type = must_traverse (T->getNamedType ());

  stack.push (mkElaboratedType (type));

  return true;
}


bool
OCamlVisitor::TraverseEnumType (clang::EnumType *T)
{
  TRACE;

  clang::StringRef name = T->getDecl ()->getName ();

  stack.push (mkEnumType (name));

  return true;
}


UNIMP_TYPE(ExtVector)
bool
OCamlVisitor::TraverseIncompleteArrayType (clang::IncompleteArrayType *T)
{
  TRACE;

  ptr<Ctyp> element = must_traverse (T->getElementType ());

  stack.push (mkIncompleteArrayType (element));

  return true;
}


UNIMP_TYPE(InjectedClassName)
UNIMP_TYPE(LValueReference)
UNIMP_TYPE(MemberPointer)
UNIMP_TYPE(ObjCInterface)
UNIMP_TYPE(ObjCObjectPointer)
UNIMP_TYPE(ObjCObject)
UNIMP_TYPE(PackExpansion)
bool
OCamlVisitor::TraverseParenType (clang::ParenType *T)
{
  TRACE;

  ptr<Ctyp> inner = must_traverse (T->getInnerType ());

  stack.push (mkParenType (inner));

  return true;
}


bool
OCamlVisitor::TraverseRecordType (clang::RecordType *T)
{
  TRACE;

  TagTypeKind kind = translate_tag_type_kind (T->getDecl ()->getTagKind ());
  clang::StringRef name = T->getDecl ()->getName ();

  stack.push (mkRecordType (kind, name));

  return true;
}


UNIMP_TYPE(RValueReference)
UNIMP_TYPE(SubstTemplateTypeParmPack)
UNIMP_TYPE(SubstTemplateTypeParm)
UNIMP_TYPE(TemplateSpecialization)
UNIMP_TYPE(TemplateTypeParm)
bool
OCamlVisitor::TraverseTypedefType (clang::TypedefType *T)
{
  TRACE;

  clang::StringRef name = T->getDecl ()->getName ();

  stack.push (mkTypedefType (name));

  return true;
}


bool
OCamlVisitor::TraverseTypeOfExprType (clang::TypeOfExprType *T)
{
  TRACE;

  ptr<Expr> expr = must_traverse (T->getUnderlyingExpr ());

  stack.push (mkTypeOfExprType (expr));

  return true;
}


bool
OCamlVisitor::TraverseTypeOfType (clang::TypeOfType *T)
{
  TRACE;

  ptr<Ctyp> type = must_traverse (T->getUnderlyingType ());

  stack.push (mkTypeOfType (type));

  return true;
}


UNIMP_TYPE(UnaryTransform)
UNIMP_TYPE(UnresolvedUsing)
bool
OCamlVisitor::TraverseVariableArrayType (clang::VariableArrayType *T)
{
  TRACE;

  ptr<Ctyp> element = must_traverse (T->getElementType ());
  ptr<Expr> size = must_traverse (T->getSizeExpr ());

  stack.push (mkVariableArrayType (element, size));

  return true;
}


UNIMP_TYPE(Vector)


// }}}

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

  // Amend with source locations.
  ptr<TypeLoc> type_loc = mkTypeLoc ();
  type_loc->tl = stack.pop ();
  type_loc->tl_sloc = sloc (TL);
  stack.push (type_loc);

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


bool
OCamlVisitor::TraverseQualifiedTypeLoc (clang::QualifiedTypeLoc TL)
{
  TRACE;

  ptr<TypeLoc> unqual = must_traverse (TL.getUnqualifiedLoc ());
  clang::Qualifiers quals = TL.getType ().getLocalQualifiers ();

  std::vector<Qualifier> qualifiers;
  if (quals.hasConst ())	qualifiers.push_back (TQ_Const);
  if (quals.hasVolatile ())	qualifiers.push_back (TQ_Volatile);
  if (quals.hasRestrict ())	qualifiers.push_back (TQ_Restrict);

  switch (quals.getObjCGCAttr ())
    {
    case clang::Qualifiers::GCNone:
      // Nothing to add.
      break;
    case clang::Qualifiers::Weak:
      qualifiers.push_back (TQ_Weak);
      break;
    case clang::Qualifiers::Strong:
      qualifiers.push_back (TQ_Strong);
      break;
    }

  switch (quals.getObjCLifetime ())
    {
    case clang::Qualifiers::OCL_None:
      // Nothing to add.
      break;
    case clang::Qualifiers::OCL_ExplicitNone:
      qualifiers.push_back (TQ_OCL_ExplicitNone);
      break;
    case clang::Qualifiers::OCL_Strong:
      qualifiers.push_back (TQ_OCL_Strong);
      break;
    case clang::Qualifiers::OCL_Weak:
      qualifiers.push_back (TQ_OCL_Weak);
      break;
    case clang::Qualifiers::OCL_Autoreleasing:
      qualifiers.push_back (TQ_OCL_Autoreleasing);
      break;
    }

  option<int> addressSpace;
  if (quals.hasAddressSpace ())
    addressSpace = quals.getAddressSpace ();

  stack.push (mkQualifiedTypeLoc (unqual, qualifiers, addressSpace));

  return true;
}


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


bool
OCamlVisitor::TraverseParenTypeLoc (clang::ParenTypeLoc TL)
{
  TRACE;

  ptr<TypeLoc> inner = must_traverse (TL.getInnerLoc ());

  stack.push (mkParenTypeLoc (inner));

  return true;
}


#define UNIMP_TYPE_LOC(CLASS)						\
bool									\
OCamlVisitor::Traverse##CLASS##TypeLoc (clang::CLASS##TypeLoc TL)	\
{									\
  TODO;									\
  TRACE;								\
  stack.push (mkUnimpTypeLoc (#CLASS));					\
  return true;								\
}

UNIMP_TYPE_LOC (Atomic)
UNIMP_TYPE_LOC (Attributed)
UNIMP_TYPE_LOC (Auto)
UNIMP_TYPE_LOC (BlockPointer)
UNIMP_TYPE_LOC (Complex)
UNIMP_TYPE_LOC (Decayed)
UNIMP_TYPE_LOC (Decltype)
UNIMP_TYPE_LOC (DependentName)
UNIMP_TYPE_LOC (DependentSizedArray)
UNIMP_TYPE_LOC (DependentSizedExtVector)
UNIMP_TYPE_LOC (DependentTemplateSpecialization)
UNIMP_TYPE_LOC (ExtVector)
UNIMP_TYPE_LOC (InjectedClassName)
UNIMP_TYPE_LOC (LValueReference)
UNIMP_TYPE_LOC (MemberPointer)
UNIMP_TYPE_LOC (ObjCInterface)
UNIMP_TYPE_LOC (ObjCObject)
UNIMP_TYPE_LOC (ObjCObjectPointer)
UNIMP_TYPE_LOC (PackExpansion)
UNIMP_TYPE_LOC (RValueReference)
UNIMP_TYPE_LOC (SubstTemplateTypeParm)
UNIMP_TYPE_LOC (SubstTemplateTypeParmPack)
UNIMP_TYPE_LOC (TemplateSpecialization)
UNIMP_TYPE_LOC (TemplateTypeParm)
UNIMP_TYPE_LOC (UnaryTransform)
UNIMP_TYPE_LOC (UnresolvedUsing)
UNIMP_TYPE_LOC (Vector)

// }}}

/****************************************************
 * {{{1 Declarations
 */

bool
OCamlVisitor::TraverseDecl (clang::Decl *D)
{
  Base::TraverseDecl (D);

  ptr<Decl> decl = mkDecl ();
  decl->d = stack.pop ();
  decl->d_sloc = sloc (D);
  stack.push (decl);

  return true;
}

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

      // Built-in implicit function declarations such as
      // printf don't have a valid TSI.
      //ptr<TypeLoc> result = mkBuiltinTypeLoc (sloc (D), BT_Void);
      //list<Decl> args;// = traverse_list (param_range (D));

      // TODO: exceptions

      //type = mkFunctionProtoTypeLoc (sloc (D), result, args);

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

bool
OCamlVisitor::TraverseEmptyDecl (clang::EmptyDecl *D)
{
  TRACE;

  stack.push (mkEmptyDecl ());

  return true;
}


bool
OCamlVisitor::TraverseTypedefDecl (clang::TypedefDecl *D)
{
  TRACE;

  ptr<TypeLoc> type = getTypeLoc (D);
  clang::StringRef name = D->getName ();

  stack.push (mkTypedefDecl (type, name));

  return true;
}


bool
OCamlVisitor::TraverseRecordDecl (clang::RecordDecl *D)
{
  TRACE;

  // Some members may be implicit (from inline unions).
  list<Decl> members = traverse_explicit_decls (D);
  clang::StringRef name = D->getName ();

  stack.push (mkRecordDecl (name, members));

  return true;
}


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


bool
OCamlVisitor::TraverseEnumDecl (clang::EnumDecl *D)
{
  TRACE;

  clang::StringRef name = D->getName ();
  list<Decl> enumerators = traverse_list (decl_range (D));

  stack.push (mkEnumDecl (name, enumerators));

  return true;
}


bool
OCamlVisitor::TraverseEnumConstantDecl (clang::EnumConstantDecl *D)
{
  TRACE;

  clang::StringRef name = D->getName ();
  option<Expr> init = maybe_traverse (D->getInitExpr ());

  stack.push (mkEnumConstantDecl (name, init));

  return true;
}


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


bool
OCamlVisitor::TraverseTranslationUnitDecl (clang::TranslationUnitDecl *D)
{
  TRACE;

  // We filter out implicit declarations before iterating.
  list<Decl> decls = traverse_explicit_decls (D);

  stack.push (mkTranslationUnitDecl (decls));

  return true;
}

#define UNIMP_DECL(CLASS)					\
  bool OCamlVisitor::Traverse##CLASS (clang::CLASS *D)		\
  {								\
    TODO;							\
    TRACE;							\
    IGNORE_ADT (CLASS, D);					\
    stack.push (mkUnimpDecl (#CLASS));				\
    return true;						\
  }


UNIMP_DECL (AccessSpecDecl)
UNIMP_DECL (BlockDecl)
UNIMP_DECL (CapturedDecl)
UNIMP_DECL (ClassScopeFunctionSpecializationDecl)
UNIMP_DECL (ClassTemplateDecl)
UNIMP_DECL (FileScopeAsmDecl)
UNIMP_DECL (FriendDecl)
UNIMP_DECL (FriendTemplateDecl)
UNIMP_DECL (FunctionTemplateDecl)
UNIMP_DECL (ImportDecl)
UNIMP_DECL (IndirectFieldDecl)
UNIMP_DECL (LabelDecl)
UNIMP_DECL (LinkageSpecDecl)
UNIMP_DECL (MSPropertyDecl)
UNIMP_DECL (NamespaceAliasDecl)
UNIMP_DECL (NamespaceDecl)
UNIMP_DECL (NonTypeTemplateParmDecl)
UNIMP_DECL (ObjCCategoryDecl)
UNIMP_DECL (ObjCCategoryImplDecl)
UNIMP_DECL (ObjCCompatibleAliasDecl)
UNIMP_DECL (ObjCImplementationDecl)
UNIMP_DECL (ObjCInterfaceDecl)
UNIMP_DECL (ObjCMethodDecl)
UNIMP_DECL (ObjCPropertyDecl)
UNIMP_DECL (ObjCPropertyImplDecl)
UNIMP_DECL (ObjCProtocolDecl)
UNIMP_DECL (OMPThreadPrivateDecl)
UNIMP_DECL (StaticAssertDecl)
UNIMP_DECL (TemplateTemplateParmDecl)
UNIMP_DECL (TemplateTypeParmDecl)
UNIMP_DECL (TypeAliasDecl)
UNIMP_DECL (TypeAliasTemplateDecl)
UNIMP_DECL (UnresolvedUsingTypenameDecl)
UNIMP_DECL (UnresolvedUsingValueDecl)
UNIMP_DECL (UsingDecl)
UNIMP_DECL (UsingDirectiveDecl)
UNIMP_DECL (UsingShadowDecl)
UNIMP_DECL (VarTemplateDecl)

// }}}


size_t
adt_of_clangAST_sharing (clang::TranslationUnitDecl const *D, ptr<Decl> &decl,
                         clang::SourceManager &SM)
{
  TIME;

  OCamlADTBase::ids_assigned = 0;
  OCamlVisitor visitor (SM);
  visitor.sharing = true;
  decl = visitor.translate (D);
  return OCamlADTBase::ids_assigned;
}


size_t
adt_of_clangAST_no_sharing (clang::TranslationUnitDecl const *D, ptr<Decl> &decl,
                            clang::SourceManager &SM)
{
  //TIME;

  OCamlADTBase::ids_assigned = 0;
  OCamlVisitor visitor (SM);
  visitor.sharing = false;
  decl = visitor.translate (D);
  return OCamlADTBase::ids_assigned;
}


ptr<Decl>
adt_of_clangAST (clang::TranslationUnitDecl const *D,
                 clang::SourceManager &SM)
{
  //D->dump ();

#if 0
  ptr<Decl> decl;
  adt_of_clangAST_no_sharing (D, decl, SM);
#else
  ptr<Decl> decl;
  adt_of_clangAST_sharing (D, decl, SM);
#endif

  return decl;
}
