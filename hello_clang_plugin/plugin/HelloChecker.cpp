//
//  HelloChecker.cpp
//  HelloClangPlugin
//
//  Created by Devin Coughlin on 10/16/13.
//  Copyright (c) 2013 Devin Coughlin. All rights reserved.
//

extern "C" {
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
}

#include <cstdio>
#include <llvm/Support/raw_ostream.h>

#include "HelloChecker.h"
#include "OCamlVisitor.h"

#include "hello_cpp.h"

using namespace clang;
using namespace ento;

void
initialize_caml ()
{
  // Make sure caml main is called once and only once
  static bool already_initialized = false;

  if (!already_initialized)
    {
      // Create fake argv on heap
      // We leak this, but since we do not know what ocaml
      // is doing with this array, it is not safe to stack-allocated
      // or free after we are done.
      static char *argv[] = {
        const_cast<char *> ("clang"),
        NULL
      };

      caml_main (argv);
      already_initialized = true;
    }
}

hello_cpp::Expr *
HelloChecker::convertExpr (const clang::Expr *in) const
{
  //Check for binary operator
  const BinaryOperator *binOp = dyn_cast<BinaryOperator> (in);

  if (binOp)
    {
      if (binOp->getOpcode () == BO_Add)
        {
          hello_cpp::BinaryOp oper = hello_cpp::BinaryOp_Add;
          return new hello_cpp::BinaryOpExpr (oper, convertExpr (binOp->getLHS ()), convertExpr (binOp->getRHS ()));
        }
      if (binOp->getOpcode () == BO_Mul)
        {
          hello_cpp::BinaryOp oper = hello_cpp::BinaryOp_Multiply;
          return new hello_cpp::BinaryOpExpr (oper, convertExpr (binOp->getLHS ()), convertExpr (binOp->getRHS ()));
        }
      return NULL;    //Return null if not defined
    }

  //Check for integer literal
  const IntegerLiteral *intLit = dyn_cast<IntegerLiteral> (in);
  if (intLit)
    {
      int value = intLit->getValue ().getSExtValue ();
      return new hello_cpp::IntConstExpr (value);
    }
  return NULL;   //Return null if something other than int lit or binop is encountered
}

void
HelloChecker::checkASTDecl (const TranslationUnitDecl *D,
                            AnalysisManager &Mgr,
                            BugReporter &BR) const
{
  CAMLparam0 ();
  CAMLlocal1 (caml_expr);

  initialize_caml ();

  value *caml_print = caml_named_value ("Hello print expr");
  caml_expr = adt_of_clangAST (D)->ToValue ();
  caml_callback (*caml_print, caml_expr);

#if 0
  clang::DeclContext::decl_iterator current;
  current = D->decls_begin ();
  for (current = D->decls_begin (); current != D->decls_end (); current++)
    {
      Decl *c = *current;
      FunctionDecl *fCastTry = dyn_cast<FunctionDecl> (c);
      if (fCastTry)
        //Get main function
        if (fCastTry->isMain ())
          {
            clang::Stmt *mainBody = fCastTry->getBody ();
            CompoundStmt *compoundStmt = dyn_cast<CompoundStmt> (mainBody);

            if (compoundStmt)
              {
                clang::Stmt **currentSt;
                for (currentSt = compoundStmt->body_begin (); currentSt != compoundStmt->body_end (); currentSt++)
                  {
                    ReturnStmt *returnStmt = dyn_cast<ReturnStmt> (*currentSt);
                    if (returnStmt)
                      {
                        clang::Expr *retVal = returnStmt->getRetValue ();
                        value *caml_print;
                        caml_print = caml_named_value ("Hello print expr");
                        hello_cpp::Expr *val = convertExpr (retVal);
                        caml_expr = val->ToValue ();
                        printf ("%p\n", caml_print);
                        caml_callback (*caml_print, caml_expr);
                      }
                  }
              }
          }
    }
#endif

  CAMLreturn0;
}
