extern "C" {
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
}

#include <clang/StaticAnalyzer/Core/PathSensitive/AnalysisManager.h>

#include <cstdio>

#include "OCamlChecker.h"
#include "ast_bridge.h"
#include "ast_bridge_of.h"
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
to_value (clang::TranslationUnitDecl const *D, clang_context &ctx)
{
  //TIME;

  ptr<ast_bridge::Decl> decl
    = ast_bridge_of<ast_bridge::Decl>
        (const_cast<clang::TranslationUnitDecl *> (D), ctx);
  ctx.values.resize (decl->id);
  return decl->to_value (ctx.values);;
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

      clang_context ctx {
        Mgr.getASTContext (),
      };

      result = to_value (D, ctx);

      OCamlADTBase::print_statistics ();

      // If this fails, then sharing didn't work.
      assert (OCamlADTBase::values_created == OCamlADTBase::ids_assigned);

      clang::SourceManager &SM = ctx->getSourceManager ();
      char const *filename = SM.getFileEntryForID (SM.getMainFileID ())->getName ();

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
