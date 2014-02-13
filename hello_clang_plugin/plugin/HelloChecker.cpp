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
#include "trace.h"

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


#define HANDLE_CXX_EXN 0


static value
to_value (adt_ptr ob)
{
  OCamlADTBase::values_created = 0;
  value_of_context ctx (ob->id);
  value result = ob->to_value (ctx);
  //printf ("%lu values created\n", OCamlADTBase::values_created);

  return result;
}


void
HelloChecker::checkASTDecl (const TranslationUnitDecl *D,
                            AnalysisManager &Mgr,
                            BugReporter &BR) const
{
  CAMLparam0 ();
  CAMLlocal1 (result);
  value *cb;

  initialize_caml ();

  for (size_t loops = 0;; loops++)
    {
#if HANDLE_CXX_EXN
      try
#endif
        {
          //TIME;
          ptr<hello_cpp::Decl> decl = adt_of_clangAST (D);

          result = to_value (decl);

          cb = caml_named_value ("Hello print decl");
        }
#if HANDLE_CXX_EXN
      catch (std::exception const &e)
        {
          result = caml_copy_string (e.what ());
          cb = caml_named_value ("Hello failure");
        }
#endif

      caml_callback (*cb, result);

      printf ("\r%lu loops", loops + 1);
      fflush (stdout);
    }
  printf ("\n");

  CAMLreturn0;
}
