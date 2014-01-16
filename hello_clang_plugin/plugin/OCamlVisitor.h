#ifndef OCAML_VISITOR_H
#define OCAML_VISITOR_H

#include <clang/AST/RecursiveASTVisitor.h>

#include "hello_cpp.h"

ptr<hello_cpp::Expr> adt_of_clangAST (clang::TranslationUnitDecl const *decl);

#endif /* OCAML_VISITOR_H */
