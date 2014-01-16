extern "C" {
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
}

#include <cstdio>

#include "OCamlVisitor.h"

using namespace hello_cpp;


struct OCamlVisitor
  : clang::RecursiveASTVisitor<OCamlVisitor>
{
private:
  typedef clang::RecursiveASTVisitor<OCamlVisitor> Base;

  std::vector<ptr<Expr>> expr_stack;

  template<typename T>
  static ptr<T>
  pop (std::vector<ptr<T>> &stack)
  {
    if (stack.empty ())
      failwith ("empty stack");
    assert (!stack.empty ());
    ptr<T> p = stack.back ();
    stack.pop_back ();
    return p;
  }

  template<typename T, typename Derived>
  static void
  push (std::vector<ptr<T>> &stack, ptr<Derived> p)
  {
    stack.push_back (p);
  }


public:
  bool TraverseBinAdd (clang::BinaryOperator *op)
  {
    Base::TraverseBinAdd (op);

    ptr<Expr> rhs = pop (expr_stack);
    ptr<Expr> lhs = pop (expr_stack);

    push (expr_stack, mkBinaryOp
          (BinaryOp_Add, lhs, rhs));

    return true;
  }

  bool TraverseBinMul (clang::BinaryOperator *op)
  {
    Base::TraverseBinMul (op);

    ptr<Expr> rhs = pop (expr_stack);
    ptr<Expr> lhs = pop (expr_stack);

    push (expr_stack, mkBinaryOp
          (BinaryOp_Multiply, lhs, rhs));

    return true;
  }

  bool TraverseIntegerLiteral (clang::IntegerLiteral *lit)
  {
    Base::TraverseIntegerLiteral (lit);

    push (expr_stack, mkIntConst
          (lit->getValue ().getSExtValue ()));

    return true;
  }


  ptr<Expr> result () const
  {
    assert (expr_stack.size () == 1);
    return expr_stack.back ();
  }
};


ptr<Expr>
adt_of_clangAST (clang::TranslationUnitDecl const *D)
{
  OCamlVisitor visitor;
  visitor.TraverseDecl (const_cast<clang::TranslationUnitDecl *> (D));
  return visitor.result ();
}
