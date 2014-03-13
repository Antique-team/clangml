#ifndef CLANG_TYPE_TRAITS_H
#define CLANG_TYPE_TRAITS_H

namespace clang
{
  class Decl;
  class Expr;
  class Stmt;
  class QualType;
  class TypeLoc;
}

namespace ast_bridge
{
  struct Decl;
  struct Expr;
  struct Stmt;
  struct Ctyp;
  struct Tloc;
}

template<typename T> struct clang_type;
template<> struct clang_type<ast_bridge::Decl> { typedef clang::Decl    *type; };
template<> struct clang_type<ast_bridge::Expr> { typedef clang::Expr    *type; };
template<> struct clang_type<ast_bridge::Stmt> { typedef clang::Stmt    *type; };
template<> struct clang_type<ast_bridge::Ctyp> { typedef clang::QualType type; };
template<> struct clang_type<ast_bridge::Tloc> { typedef clang::TypeLoc  type; };

template<typename T> struct bridge_type;
template<> struct bridge_type<clang::Decl    *> { typedef ast_bridge::Decl type; };
template<> struct bridge_type<clang::Expr    *> { typedef ast_bridge::Expr type; };
template<> struct bridge_type<clang::Stmt    *> { typedef ast_bridge::Stmt type; };
template<> struct bridge_type<clang::QualType > { typedef ast_bridge::Ctyp type; };
template<> struct bridge_type<clang::TypeLoc  > { typedef ast_bridge::Tloc type; };

#endif /* CLANG_TYPE_TRAITS_H */
