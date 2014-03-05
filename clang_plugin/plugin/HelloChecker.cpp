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

#include "bridge_ast.h"

using namespace clang;
using namespace ento;

void
initialize_caml ()
{
  // Make sure caml main is called once and only once
  static bool already_initialized = false;

  if (!already_initialized)
    {
      // Create fake argv
      static char *argv[] = {
        const_cast<char *> ("clang"),
        NULL
      };

      caml_main (argv);
      already_initialized = true;
    }
}


static value
to_value (adt_ptr ob)
{
  value_of_context ctx (ob->id);
  value result = ob->to_value (ctx);

  return result;
}


void
HelloChecker::checkASTDecl (const TranslationUnitDecl *D,
                            AnalysisManager &Mgr,
                            BugReporter &BR) const
{
  CAMLparam0 ();
  CAMLlocal1 (result);

  initialize_caml ();

#define HANDLE_CXX_EXN 0
#if HANDLE_CXX_EXN
  try
#endif
    {
      //TIME;

      OCamlADTBase::reset_statistics ();

      SourceManager &SM = BR.getSourceManager ();
      ptr<bridge_ast::Decl> decl = adt_of_clangAST (D, SM);

      result = to_value (decl);

      OCamlADTBase::print_statistics ();

      // If this fails, then sharing didn't work.
      assert (OCamlADTBase::values_created == OCamlADTBase::ids_assigned);

      char const *filename = SM.getFileEntryForID (SM.getMainFileID ())->getName ();

      value *cb = caml_named_value ("success");
      caml_callback2 (*cb, result, caml_copy_string (filename));
    }
#if HANDLE_CXX_EXN
  catch (std::exception const &e)
    {
      result = caml_copy_string (e.what ());
      value *cb = caml_named_value ("failure");
      caml_callback (*cb, result);
    }
#endif

  CAMLreturn0;
}
