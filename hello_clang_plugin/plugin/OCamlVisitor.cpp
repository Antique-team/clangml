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

#include "OCamlVisitor.h"

using namespace hello_cpp;


struct OCamlVisitor
  : clang::RecursiveASTVisitor<OCamlVisitor>
{
private:
  typedef clang::RecursiveASTVisitor<OCamlVisitor> Base;

  std::vector<adt_ptr> stack;

  struct dynamic
  {
    adt_ptr adt;

    template<typename T>
    operator ptr<T> () const
    {
      ptr<T> p = boost::dynamic_pointer_cast<T> (adt);
      assert (p);
      return p;
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

  template<typename Derived>
  void push (ptr<Derived> p)
  {
    static_assert (std::is_base_of<OCamlADTBase, Derived>::value,
                   "can only push OCamlADTBase derived instances");
    stack.push_back (p);
  }


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
   * Statements
   */

#define ABSTRACT_STMT(STMT)
#define STMT(CLASS, BASE)					\
  bool Traverse##CLASS (clang::CLASS *stmt)			\
  {								\
    puts (__func__);						\
    Base::Traverse##CLASS (stmt);				\
    return true;						\
  }
#include <clang/AST/StmtNodes.inc>
#undef TYPE


  /****************************************************
   * Types
   */

#define ABSTRACT_TYPE(CLASS, BASE)
#define TYPE(CLASS, BASE)					\
  bool Traverse##CLASS##Type (clang::CLASS##Type *type)		\
  {								\
    puts (__func__);						\
    Base::Traverse##CLASS##Type (type);				\
    return true;						\
  }
#include <clang/AST/TypeNodes.def>
#undef TYPE


  /****************************************************
   * TypeLocs
   */

#define ABSTRACT_TYPELOC(CLASS, BASE)
#define TYPELOC(CLASS, BASE)					\
  bool Traverse##CLASS##TypeLoc (clang::CLASS##TypeLoc typeLoc)	\
  {								\
    puts (__func__);						\
    Base::Traverse##CLASS##TypeLoc (typeLoc);			\
    return true;						\
  }
#include <clang/AST/TypeLocNodes.def>
#undef TYPE


  /****************************************************
   * Declarations
   */

#define ABSTRACT_DECL(DECL)
#define DECL(CLASS, BASE)					\
  bool Traverse##CLASS##Decl (clang::CLASS##Decl *decl)		\
  {								\
    puts (__func__);						\
    Base::Traverse##CLASS##Decl (decl);				\
    return true;						\
  }
#include <clang/AST/DeclNodes.inc>
#undef DECL


  /****************************************************
   * Final result
   */

  ptr<Expr> result ()
  {
    //assert (stack.size () == 1);
    return pop ();
  }
};


ptr<Expr>
adt_of_clangAST (clang::TranslationUnitDecl const *D)
{
  OCamlVisitor visitor;
  visitor.TraverseDecl (const_cast<clang::TranslationUnitDecl *> (D));
  return visitor.result ();
}
