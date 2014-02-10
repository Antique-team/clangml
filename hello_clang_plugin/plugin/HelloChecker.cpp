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

void
HelloChecker::checkASTDecl (const TranslationUnitDecl *D,
                            AnalysisManager &Mgr,
                            BugReporter &BR) const
{
  CAMLparam0 ();
  CAMLlocal1 (result);
  value *cb;

  initialize_caml ();

#if HANDLE_CXX_EXN
  try
#endif
  {
    TIME;
    result = adt_of_clangAST (D)->ToValue ();
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

  CAMLreturn0;
}
