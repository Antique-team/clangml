#ifndef CLANG_TYPE_TRAITS_H
#define CLANG_TYPE_TRAITS_H

namespace clang
{
  struct Decl;
  struct Expr;
  struct Stmt;
  struct QualType;
  struct TypeLoc;
}

namespace bridge_ast
{
  struct Decl;
  struct Expr;
  struct Stmt;
  struct Ctyp;
  struct Tloc;
}

template<typename T> struct clang_type;
template<> struct clang_type<bridge_ast::Decl> { typedef clang::Decl    *type; };
template<> struct clang_type<bridge_ast::Expr> { typedef clang::Expr    *type; };
template<> struct clang_type<bridge_ast::Stmt> { typedef clang::Stmt    *type; };
template<> struct clang_type<bridge_ast::Ctyp> { typedef clang::QualType type; };
template<> struct clang_type<bridge_ast::Tloc> { typedef clang::TypeLoc  type; };

template<typename T> struct bridge_type;
template<> struct bridge_type<clang::Decl    *> { typedef bridge_ast::Decl type; };
template<> struct bridge_type<clang::Expr    *> { typedef bridge_ast::Expr type; };
template<> struct bridge_type<clang::Stmt    *> { typedef bridge_ast::Stmt type; };
template<> struct bridge_type<clang::QualType > { typedef bridge_ast::Ctyp type; };
template<> struct bridge_type<clang::TypeLoc  > { typedef bridge_ast::Tloc type; };

#endif /* CLANG_TYPE_TRAITS_H */
