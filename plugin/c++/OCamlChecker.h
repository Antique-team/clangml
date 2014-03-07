//
//  Created by Devin Coughlin on 10/16/13.
//  Copyright (c) 2013 Devin Coughlin. All rights reserved.
//

#ifndef OCAML_CHECKER_H
#define OCAML_CHECKER_H

#include <clang/StaticAnalyzer/Core/Checker.h>
#include <clang/StaticAnalyzer/Core/PathSensitive/CheckerContext.h>

#include <clang/AST/DeclObjC.h>
#include <clang/AST/ExprCXX.h>
#include <clang/AST/Expr.h>
#include <clang/AST/ExprObjC.h>
#include <clang/AST/StmtObjC.h>

#include "bridge_ast.h"

struct OCamlChecker
  : clang::ento::Checker<clang::ento::check::ASTDecl<clang::TranslationUnitDecl> >
{
  void checkASTDecl (const clang::TranslationUnitDecl *D,
                     clang::ento::AnalysisManager &Mgr,
                     clang::ento::BugReporter &BR) const;
};


#endif /* OCAML_CHECKER_H */
