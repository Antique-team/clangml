//
//  HelloChecker.h
//  HelloClangPlugin
//
//  Created by Devin Coughlin on 10/16/13.
//  Copyright (c) 2013 Devin Coughlin. All rights reserved.
//

#ifndef __HelloClangPlugin__HelloChecker__
#define __HelloClangPlugin__HelloChecker__

#include <clang/StaticAnalyzer/Core/Checker.h>

#include <clang/StaticAnalyzer/Core/PathSensitive/CheckerContext.h>


#include <clang/AST/ExprObjC.h>
#include <clang/AST/StmtObjC.h>
#include <clang/AST/ExprCXX.h>
#include <clang/AST/DeclObjC.h>
#include <clang/AST/Expr.h>
#include "hello_cpp.h"

class HelloChecker
  : public clang::ento::Checker<clang::ento::check::ASTDecl<clang::TranslationUnitDecl> >
{
public:
  hello_cpp::Expr *convertExpr (const clang::Expr *in) const;
  void checkASTDecl   (const clang::TranslationUnitDecl *D, clang::ento::AnalysisManager &Mgr, clang::ento::BugReporter &BR) const;
};



#endif /* defined(__HelloClangPlugin__HelloChecker__) */
