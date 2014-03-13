#include "ast_bridge_of.h"

#include "OCamlVisitor/OCamlVisitor.h"

template<typename T>
ptr<T>
ast_bridge_of (typename clang_type<T>::type N,
               clang_context &ctx)
{
  //D->dump ();

  //TIME;

  OCamlVisitor visitor (ctx);
  return visitor.translate (N);
}

template ptr<Decl> ast_bridge_of<Decl> (clang::Decl *  , clang_context &ctx);
template ptr<Expr> ast_bridge_of<Expr> (clang::Expr *  , clang_context &ctx);
template ptr<Stmt> ast_bridge_of<Stmt> (clang::Stmt *  , clang_context &ctx);
template ptr<Ctyp> ast_bridge_of<Ctyp> (clang::QualType, clang_context &ctx);
template ptr<Tloc> ast_bridge_of<Tloc> (clang::TypeLoc , clang_context &ctx);
