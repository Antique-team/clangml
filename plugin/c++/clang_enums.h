#ifndef CLANG_ENUMS_H
#define CLANG_ENUMS_H

#include <clang/AST/Expr.h>

#include "ast_bridge.h"

ast_bridge::UnaryOperator translate_unary_operator_kind (clang::UnaryOperatorKind kind);
ast_bridge::BinaryOperator translate_binary_operator_kind (clang::BinaryOperatorKind kind);
ast_bridge::PredefinedExpr translate_predefined_expr (clang::PredefinedExpr::IdentType kind);
ast_bridge::TagTypeKind translate_tag_type_kind (clang::TagTypeKind kind);
ast_bridge::AttributedTypeKind translate_attributed_type_kind (clang::AttributedType::Kind kind);
ast_bridge::VectorKind translate_vector_kind (clang::VectorType::VectorKind kind);
ast_bridge::ElaboratedTypeKeyword translate_elaborated_type_keyword (clang::ElaboratedTypeKeyword kw);
ast_bridge::BuiltinType translate_builtin_type (clang::BuiltinType::Kind kind);
ast_bridge::CastKind translate_cast_kind (clang::CastKind kind);
ast_bridge::AtomicOp translate_atomic_op (clang::AtomicExpr::AtomicOp ao);
ast_bridge::AccessSpecifier translate_access_specifier (clang::AccessSpecifier spec);
ast_bridge::OverloadedOperatorKind translate_overloaded_operator_kind (clang::OverloadedOperatorKind kind);
ast_bridge::BinaryTypeTrait translate_binary_type_trait (clang::BinaryTypeTrait btt);

#endif /* CLANG_ENUMS_H */
