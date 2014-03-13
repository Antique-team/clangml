#ifndef OCAML_CHECKER_H
#define OCAML_CHECKER_H

#include <clang/StaticAnalyzer/Core/Checker.h>

#include "ast_bridge.h"

struct OCamlChecker
  : clang::ento::Checker<clang::ento::check::ASTDecl<clang::TranslationUnitDecl> >
{
  void checkASTDecl (const clang::TranslationUnitDecl *D,
                     clang::ento::AnalysisManager &Mgr,
                     clang::ento::BugReporter &BR) const;
};


#endif /* OCAML_CHECKER_H */
