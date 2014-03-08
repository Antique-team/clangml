extern "C" {
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
}

#include <clang/StaticAnalyzer/Core/BugReporter/BugReporter.h>

#include <cstdio>

#include "OCamlChecker.h"
#include "OCamlVisitor.h"
#include "bridge_ast.h"
#include "clang_context.h"
#include "trace.h"

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

static value
to_value (clang::TranslationUnitDecl const *D, clang_context &ctx)
{
  ptr<bridge_ast::Decl> decl
    = bridge_ast_of<bridge_ast::Decl>
        (const_cast<clang::TranslationUnitDecl *> (D), ctx);
  return to_value (decl);
}


void
OCamlChecker::checkASTDecl (clang::TranslationUnitDecl const *D,
                            clang::ento::AnalysisManager &Mgr,
                            clang::ento::BugReporter &BR) const
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

      clang_context ctx = {
        BR.getSourceManager (),
      };

      result = to_value (D, ctx);

      OCamlADTBase::print_statistics ();

      // If this fails, then sharing didn't work.
      assert (OCamlADTBase::values_created == OCamlADTBase::ids_assigned);

      char const *filename = ctx.SM.getFileEntryForID (ctx.SM.getMainFileID ())->getName ();

      value *cb = caml_named_value ("success");
      // val success : decl -> string -> context -> unit
      caml_callback3 (*cb,
                      result,
                      caml_copy_string (filename),
                      Val_bp (&ctx));
      // After the above call, all C++ values will be destroyed and
      // should no longer be used.
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
