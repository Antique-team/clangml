//
//  Created by Devin Coughlin on 10/16/13.
//  Copyright (c) 2013 Devin Coughlin. All rights reserved.
//

extern "C" {
#include <caml/alloc.h>
#include <caml/fail.h>
}

#include <clang/StaticAnalyzer/Core/CheckerRegistry.h>

#include "OCamlChecker.h"

using clang::ento::CheckerRegistry;

// Register plugin!
O_EXPORT void clang_registerCheckers (CheckerRegistry &registry);
O_EXPORT char const clang_analyzerAPIVersionString[] = CLANG_ANALYZER_API_VERSION_STRING;

void
clang_registerCheckers (CheckerRegistry &registry)
{
  registry.addChecker<OCamlChecker> ("OCamlChecker", "RunOCamlChecker");
}
