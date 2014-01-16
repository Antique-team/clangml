//
//  PluginRegistration.cpp
//  HelloClangPlugin
//
//  Created by Devin Coughlin on 10/16/13.
//  Copyright (c) 2013 Devin Coughlin. All rights reserved.
//


#include <clang/StaticAnalyzer/Core/CheckerRegistry.h>

#include "HelloChecker.h"

using namespace clang;
using namespace ento;

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
