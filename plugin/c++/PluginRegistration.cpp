#include <clang/StaticAnalyzer/Core/CheckerRegistry.h>

#include "backtrace.h"
#include "OCamlChecker.h"

#include <sys/param.h> // OS detection

O_EXPORT void clang_registerCheckers (clang::ento::CheckerRegistry &registry);
O_EXPORT char const clang_analyzerAPIVersionString[] = CLANG_ANALYZER_API_VERSION_STRING;

void
clang_registerCheckers (clang::ento::CheckerRegistry &registry)
{

#ifndef __FreeBSD__
#ifndef __APPLE__
  backtrace_init ();
#endif // __APPLE__
#endif  // __FreeBSD__
  registry.addChecker<OCamlChecker> ("external.OCaml", "Runs static analysis passes written in OCaml");
}
