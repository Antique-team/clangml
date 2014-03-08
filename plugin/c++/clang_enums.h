#ifndef CLANG_ENUMS_H
#define CLANG_ENUMS_H

#include <clang/AST/Expr.h>

#include "bridge_ast.h"

bridge_ast::PredefinedExpr translate_predefined_expr (clang::PredefinedExpr::IdentType kind);
bridge_ast::TagTypeKind translate_tag_type_kind (clang::TagTypeKind kind);
bridge_ast::ElaboratedTypeKeyword translate_elaborated_type_keyword (clang::ElaboratedTypeKeyword kw);
bridge_ast::BuiltinType translate_builtin_type (clang::BuiltinType::Kind kind);
bridge_ast::CastKind translate_cast_kind (clang::CastKind kind);

#endif /* CLANG_ENUMS_H */
