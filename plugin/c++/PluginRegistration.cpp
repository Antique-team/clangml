#include <clang/StaticAnalyzer/Core/CheckerRegistry.h>

#include "OCamlChecker.h"

O_EXPORT void clang_registerCheckers (clang::ento::CheckerRegistry &registry);
O_EXPORT char const clang_analyzerAPIVersionString[] = CLANG_ANALYZER_API_VERSION_STRING;

void
clang_registerCheckers (clang::ento::CheckerRegistry &registry)
{
  registry.addChecker<OCamlChecker> ("OCamlChecker", "RunOCamlChecker");
}
