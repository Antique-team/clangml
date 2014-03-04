#ifndef OCAML_VISITOR_H
#define OCAML_VISITOR_H

#include <clang/AST/RecursiveASTVisitor.h>

#include "bridge_ast.h"

ptr<bridge_ast::Decl> adt_of_clangAST (clang::TranslationUnitDecl const *D,
                                      clang::SourceManager &SM);

#endif /* OCAML_VISITOR_H */
