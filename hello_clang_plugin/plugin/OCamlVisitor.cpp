extern "C" {
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
}

#include <cstdio>

#include "OCamlVisitor.h"

using namespace clang;


struct OCamlVisitor
  : clang::RecursiveASTVisitor<OCamlVisitor>
{
private:
  std::vector<hello_cpp::Expr *> expr_stack;

  template<typename T>
  static T *
  pop (std::vector<T *> &stack)
  {
    if (stack.empty ())
      failwith ("empty stack");
    assert (!stack.empty ());
    T *p = stack.back ();
    stack.pop_back ();
    return p;
  }

  template<typename T, typename Derived>
  static void
  push (std::vector<T *> &stack, Derived *p)
  {
    stack.push_back (p);
  }


public:
  bool TraverseBinAdd (BinaryOperator *op)
  {
    RecursiveASTVisitor::TraverseBinAdd (op);

    hello_cpp::Expr *rhs = pop (expr_stack);
    hello_cpp::Expr *lhs = pop (expr_stack);

    push (expr_stack, new hello_cpp::BinaryOpExpr
          (hello_cpp::BinaryOp_Add, lhs, rhs));

    return true;
  }

  bool TraverseBinMul (BinaryOperator *op)
  {
    RecursiveASTVisitor::TraverseBinMul (op);

    hello_cpp::Expr *rhs = pop (expr_stack);
    hello_cpp::Expr *lhs = pop (expr_stack);

    push (expr_stack, new hello_cpp::BinaryOpExpr
          (hello_cpp::BinaryOp_Multiply, lhs, rhs));

    return true;
  }

  bool TraverseIntegerLiteral (IntegerLiteral *lit)
  {
    RecursiveASTVisitor::TraverseIntegerLiteral (lit);

    push (expr_stack, new hello_cpp::IntConstExpr
          (lit->getValue ().getSExtValue ()));

    return true;
  }


  hello_cpp::Expr *result () const
  {
    assert (expr_stack.size () == 1);
    return expr_stack.back ();
  }
};


hello_cpp::Expr *
adt_of_clangAST (TranslationUnitDecl const *D)
{
  OCamlVisitor visitor;
  visitor.TraverseDecl (const_cast<TranslationUnitDecl *> (D));
  return visitor.result ();
}
