//
//  PluginRegistration.cpp
//  HelloClangPlugin
//
//  Created by Devin Coughlin on 10/16/13.
//  Copyright (c) 2013 Devin Coughlin. All rights reserved.
//

extern "C" {
#include <caml/alloc.h>
#include <caml/fail.h>
}

#include <clang/StaticAnalyzer/Core/CheckerRegistry.h>

#include "HelloChecker.h"

using clang::ento::CheckerRegistry;

// Register plugin!
extern "C"
void clang_registerCheckers (CheckerRegistry &registry);

extern "C"
void
clang_registerCheckers (CheckerRegistry &registry)
{
  registry.addChecker<HelloChecker> ("coughlin.HelloChecker", "RunHelloChecker");
}

extern "C"
char const clang_analyzerAPIVersionString[] = CLANG_ANALYZER_API_VERSION_STRING;

extern "C"
CAMLprim value
check_bridge_version (value version)
{
  if (strcmp (String_val (version), bridge_ast::version) != 0)
    {
      char buf[128];
      snprintf (buf, sizeof buf, "Bridge version mismatch: OCaml side is \"%s\", C++ side is \"%s\"",
                String_val (version), bridge_ast::version);
      failwith (buf);
    }
  return Val_unit;
}
