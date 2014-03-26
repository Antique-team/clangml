#include <clang/StaticAnalyzer/Core/CheckerRegistry.h>

#include "backtrace.h"
#include "OCamlChecker.h"

O_EXPORT void clang_registerCheckers (clang::ento::CheckerRegistry &registry);
O_EXPORT char const clang_analyzerAPIVersionString[] = CLANG_ANALYZER_API_VERSION_STRING;

void
clang_registerCheckers (clang::ento::CheckerRegistry &registry)
{
  backtrace_init ();
  registry.addChecker<OCamlChecker> ("external.OCaml", "Runs static analysis passes written in OCaml");
}
