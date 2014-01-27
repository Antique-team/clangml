extern "C" {
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
}

#include <cstdio>
#include <stdexcept>
#include <type_traits>
#include <typeinfo>

#include "OCamlVisitor.h"

using namespace hello_cpp;


#include <cxxabi.h>

static std::string
demangle (std::type_info const &ti)
{
  size_t length;
  int status;
  if (char *name = __cxxabiv1::__cxa_demangle (ti.name (), NULL, &length, &status))
    {
      std::string result (name, length);
      free (name);
      return result;
    }
#if 0
  switch (status)
    {
    case -1: return "A memory allocation failure occurred.";
    case -2: return "Type name is not a valid name under the C++ ABI mangling rules.";
    case -3: return "One of the arguments is invalid.";
    }
#endif
  return ti.name ();
}

template<typename T>
static std::string
name (T const &p)
{
  return demangle (typeid (p));
}

template<typename T>
static std::string
name ()
{
  return demangle (typeid (T));
}


static unsigned int level = 0;
struct tracer
{
  tracer (char const *func)
    : func (func)
  {
    printf ("%*s> %s\n", level++, "", func);
  }

  ~tracer ()
  {
    printf ("%*s< %s\n", --level, "", func);
  }

  char const *func;
};

#define TRACE tracer trace (__func__)
#define TODO printf ("TODO: %s\n", __func__)

struct OCamlVisitor
  : clang::RecursiveASTVisitor<OCamlVisitor>
{
private:
  typedef clang::RecursiveASTVisitor<OCamlVisitor> Base;

  /****************************************************
   * Stack management
   */

  std::vector<adt_ptr> stack;
  std::vector<size_t> markers;

  template<typename T>
  static ptr<T> adt_cast (adt_ptr adt)
  {
    ptr<T> p = boost::dynamic_pointer_cast<T> (adt);
    if (!p)
      {
        printf ("Expected type %s, but actually type %s\n",
                name<T> ().c_str (), name (*adt).c_str ());
        throw std::bad_cast ();
      }
    return p;
  }

  struct dynamic
  {
    adt_ptr adt;

    template<typename T>
    operator ptr<T> () const
    {
      assert (adt);
      return adt_cast<T> (adt);
    }

    template<typename T>
    operator option<T> () const
    {
      if (!adt)
        return nullptr;
      return adt_cast<T> (adt);
    }
  };

  struct dynamic_list
  {
    std::vector<adt_ptr> adt_list;

    template<typename T>
    operator std::vector<ptr<T>> () const
    {
      std::vector<ptr<T>> list;
      list.reserve (adt_list.size ());
      std::transform (adt_list.begin (), adt_list.end (),
                      std::back_inserter (list),
                      adt_cast<T>);
      return list;
    }
  };

  dynamic pop ()
  {
    if (stack.empty ())
      throw std::runtime_error ("empty stack");
    assert (!stack.empty ());
    adt_ptr p = stack.back ();
    stack.pop_back ();

    return dynamic { p };
  }

  template<template<typename> class Ptr, typename Derived>
  void push (Ptr<Derived> p)
  {
    // Requiring this specific template already makes pushing option
    // impossible, because it's option<typename, bool>, but just in case
    // that changes in the future, we explicitly check for it here.
    static_assert (!std::is_base_of<option<Derived>, Ptr<Derived>>::value,
                   "cannot push option types");
    // The push_back below would fail if this is not the case, but this
    // gives a more descriptive error message.
    static_assert (std::is_base_of<OCamlADTBase, Derived>::value,
                   "can only push OCaml bridge types");
    stack.push_back (p);
  }


  // Set a stack marker.
  void push_mark ()
  {
    markers.push_back (stack.size ());
  }

  // Get the number of elements pushed since the last marker.
  size_t pop_mark ()
  {
    size_t marker = markers.back ();
    assert (marker <= stack.size ());
    markers.pop_back ();
    return stack.size () - marker;
  }

  // Get a list of everything that was pushed since the last marker.
  dynamic_list pop_marked ()
  {
    // Get last marker.
    size_t marker = pop_mark ();

    // Copy the last size-marker elements to the list.
    std::vector<adt_ptr> list;
    list.reserve (marker);
    list.insert (list.begin (), stack.end () - marker, stack.end ());

    // Pop them off the stack.
    stack.erase (stack.end () - marker, stack.end ());

    return dynamic_list { move (list) };
  }


  // Return null if p is null, otherwise call fun and return
  // its result.
  template<typename T, typename Fun>
  static dynamic maybe (T *p, Fun fun)
  {
    if (!p)
      return dynamic { };
    return dynamic { fun (p) };
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


  /****************************************************
   * Unary/binary operators (helpers)
   */

  void consume_unaryop (UnaryOp op)
  {
    ptr<Expr> arg = pop ();

    push (mkUnaryOperator
          (op, arg));
  }


  void consume_binop (BinaryOp op)
  {
    ptr<Expr> rhs = pop ();
    ptr<Expr> lhs = pop ();

    push (mkBinaryOperator
          (op, lhs, rhs));
  }

public:
  /****************************************************
   * Unary/binary operators
   */

// Code taken from <clang/AST/RecursiveASTVisitor.h>

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
  bool TraverseUnary##OP (clang::UnaryOperator *op)			\
  {									\
    Base::TraverseUnary##OP (op);					\
    consume_unaryop (UO_##OP);						\
    return true;							\
  }

  UNARYOP_LIST ()
#undef OPERATOR


#define OPERATOR(OP)							\
  bool TraverseBin##OP (clang::BinaryOperator *op)			\
  {									\
    Base::TraverseBin##OP (op);						\
    consume_binop (BO_##OP);						\
    return true;							\
  }

  BINOP_LIST ()
#undef OPERATOR


#define OPERATOR(OP)							\
  bool TraverseBin##OP##Assign (clang::CompoundAssignOperator *op)	\
  {									\
    Base::TraverseBin##OP##Assign (op);					\
    consume_binop (BO_##OP##Assign);					\
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
    Base::TraverseIntegerLiteral (lit);

    push (mkIntegerLiteral
          (lit->getValue ().getSExtValue ()));

    return true;
  }


#define CHARACTERLITERAL(CLASS, BASE)
  bool TraverseCharacterLiteral (clang::CharacterLiteral *lit)
  {
    Base::TraverseCharacterLiteral (lit);

    push (mkCharacterLiteral
          (lit->getValue ()));

    return true;
  }


#define FLOATINGLITERAL(CLASS, BASE)
  bool TraverseFloatingLiteral (clang::FloatingLiteral *lit)
  {
    Base::TraverseFloatingLiteral (lit);

    push (mkFloatingLiteral
          (lit->getValue ().convertToDouble ()));

    return true;
  }


#define STRINGLITERAL(CLASS, BASE)
  bool TraverseStringLiteral (clang::StringLiteral *lit)
  {
    Base::TraverseStringLiteral (lit);

    push (mkStringLiteral
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
    Base::TraverseDeclRefExpr (S);

    clang::StringRef name = getName (S);
    push (mkDeclRefExpr (name));

    return true;
  }


#define IMPLICITCASTEXPR(CLASS, BASE)
  bool TraverseImplicitCastExpr (clang::ImplicitCastExpr *S)
  {
    TRACE;
    Base::TraverseImplicitCastExpr (S);

    ptr<Expr> inner = pop ();

    push (mkImplicitCastExpr (inner));

    return true;
  }


#define PARENEXPR(CLASS, BASE)
  bool TraverseParenExpr (clang::ParenExpr *S)
  {
    TRACE;
    Base::TraverseParenExpr (S);

    ptr<Expr> inner = pop ();

    push (mkParenExpr (inner));

    return true;
  }

  /****************************************************
   * Statements
   */

#define IFSTMT(CLASS, BASE)
  bool TraverseIfStmt (clang::IfStmt *S)
  {
    TRACE;

    TraverseStmt (S->getCond ());
    ptr<Expr> cond = pop ();

    TraverseStmt (S->getThen ());
    ptr<Stmt> thenBranch = pop ();

    option<Stmt> elseBranch =
      maybe (S->getElse (), [this] (clang::Stmt *S) {
               TraverseStmt (S);
               return pop ();
             });

    push (mkIfStmt (cond, thenBranch, elseBranch));

    return true;
  }

#define RETURNSTMT(CLASS, BASE)
  bool TraverseReturnStmt (clang::ReturnStmt *S)
  {
    TRACE;
    Base::TraverseReturnStmt (S);

    ptr<Expr> expr = pop ();
    push (mkReturnStmt (expr));

    return true;
  }


#define COMPOUNDSTMT(CLASS, BASE)
  bool TraverseCompoundStmt (clang::CompoundStmt *S)
  {
    TRACE;
    push_mark ();
    Base::TraverseCompoundStmt (S);

    std::vector<ptr<Stmt>> stmts = pop_marked ();
    push (mkCompoundStmt (stmts));

    return true;
  }


#define DECLSTMT(CLASS, BASE)
  bool TraverseDeclStmt (clang::DeclStmt *S)
  {
    TRACE;
    Base::TraverseDeclStmt (S);

    ptr<Decl> decl = pop ();
    push (mkDeclStmt (decl));

    return true;
  }


#define IGNORE_ADT(CLASS, VAR)					\
    push_mark ();						\
    Base::Traverse##CLASS (VAR);				\
    /* Drop everything made by previous calls. */		\
    size_t marker = pop_mark ();				\
    while (marker--) pop ();


#define ABSTRACT_STMT(STMT)
#define STMT(CLASS, BASE)					\
  bool Traverse##CLASS (clang::CLASS *S)			\
  {								\
    TRACE;							\
    IGNORE_ADT (CLASS, S);					\
    push (mkUnimpStmt (#CLASS));				\
    return true;						\
  }
#define EXPR(CLASS, BASE)					\
  bool Traverse##CLASS (clang::CLASS *S)			\
  {								\
    TRACE;							\
    IGNORE_ADT (CLASS, S);					\
    push (mkUnimpExpr (#CLASS));				\
    return true;						\
  }
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
    push_mark ();
    Base::TraverseTypeLoc (TL);
    size_t marker = pop_mark ();
    if (marker == 0)
      {
        printf ("WARNING: %s creates dummy TypeLoc, as derived function did not produce any\n",
                __func__);
        push (mkBuiltinTypeLoc (BT_Void));
      }
    else if (marker > 1)
      {
        ptr<TypeLoc> mostRecent = pop ();
        printf ("WARNING: %s drops all but most recent (out of %lu) TypeLoc\n",
                __func__, marker);
        // Keep the last one
        while (--marker) pop ();
        push (mostRecent);
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

    push (mkBuiltinTypeLoc (bt));

    return true;
  }

  bool TraverseConstantArrayTypeLoc (clang::ConstantArrayTypeLoc TL)
  {
    TRACE;
    Base::TraverseConstantArrayTypeLoc (TL);

    ptr<TypeLoc> inner = pop ();
    uint64_t size = TL.getTypePtr ()->getSize ().getZExtValue ();

    push (mkConstantArrayTypeLoc (inner, size));

    return true;
  }

  bool TraversePointerTypeLoc (clang::PointerTypeLoc TL)
  {
    TRACE;
    Base::TraversePointerTypeLoc (TL);

    ptr<TypeLoc> inner = pop ();
    push (mkPointerTypeLoc (inner));

    return true;
  }

  bool TraverseFunctionNoProtoTypeLoc (clang::FunctionNoProtoTypeLoc TL)
  {
    TRACE;
    TraverseTypeLoc (TL.getResultLoc ());
    ptr<TypeLoc> result = pop ();

    push (mkFunctionNoProtoTypeLoc (result));

    return true;
  }

  bool TraverseFunctionProtoTypeLoc (clang::FunctionProtoTypeLoc TL)
  {
    TRACE;

    TraverseTypeLoc (TL.getResultLoc ());
    ptr<TypeLoc> result = pop ();

    clang::FunctionProtoType const *T = TL.getTypePtr ();

    push_mark ();

    for (unsigned I = 0, E = TL.getNumArgs (); I != E; ++I)
      {
        clang::ParmVarDecl *Arg = TL.getArg (I);
        assert (Arg);
        TraverseDecl (Arg);
      }

    std::vector<ptr<Decl>> args = pop_marked ();

    // TODO: exceptions

    push (mkFunctionProtoTypeLoc (result, args));

    return true;
  }

  bool TraverseTypedefTypeLoc (clang::TypedefTypeLoc TL)
  {
    TRACE;
    Base::TraverseTypedefTypeLoc (TL);

    clang::StringRef name = TL.getTypedefNameDecl ()->getName ();
    push (mkTypedefTypeLoc (name));

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
    push (mkBuiltinTypeLoc (BT_Void));				\
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
    ptr<TypeLoc> type = pop ();

    // Function body, or None.
    option<Stmt> body;
    if (D->isThisDeclarationADefinition ())
      {
        TraverseStmt (D->getBody ());
        body = pop ();
      }

    // TODO: Constructor initialisers.

    // Function name.
    clang::StringRef name = getName (D);

    push (mkFunctionDecl (type, name, body));

    return true;
  }


#define TYPEDEF(CLASS, BASE)
  bool TraverseTypedefDecl (clang::TypedefDecl *D)
  {
    TRACE;
    Base::TraverseTypedefDecl (D);

    ptr<TypeLoc> type = pop ();
    clang::StringRef name = D->getName ();

    push (mkTypedefDecl (type, name));

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
    ptr<TypeLoc> type = pop ();

    clang::StringRef name = D->getName ();

    push (mkParmVarDecl (type, name));

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
    ptr<TypeLoc> type = pop ();

    clang::StringRef name = D->getName ();

    push (mkVarDecl (type, name));

    return true;
  }


#define TRANSLATIONUNIT(CLASS, BASE)
  bool TraverseTranslationUnitDecl (clang::TranslationUnitDecl *D)
  {
    TRACE;
    push_mark ();
    Base::TraverseTranslationUnitDecl (D);

    std::vector<ptr<Decl>> decls = pop_marked ();
    push (mkTranslationUnitDecl (decls));

    return true;
  }


#define ABSTRACT_DECL(DECL)
#define DECL(CLASS, BASE)					\
  bool Traverse##CLASS##Decl (clang::CLASS##Decl *D)		\
  {								\
    TRACE;							\
    Base::Traverse##CLASS##Decl (D);				\
    IGNORE_ADT (CLASS##Decl, D);				\
    push (mkUnimpDecl (#CLASS));				\
    return true;						\
  }
#include <clang/AST/DeclNodes.inc>


  /****************************************************
   * Final result
   */

  ptr<Decl> result ()
  {
    assert (stack.size () == 1);
    return pop ();
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
