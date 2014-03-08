#include "bridge_ast_of.h"

#include "OCamlVisitor/OCamlVisitor.h"

template<typename T>
ptr<T>
bridge_ast_of (typename clang_type<T>::type N,
               clang_context &ctx)
{
  //D->dump ();

  //TIME;

  OCamlVisitor visitor (ctx);
  return visitor.translate (N);
}

template ptr<Decl>	bridge_ast_of<Decl>	(clang::Decl *  , clang_context &ctx);
template ptr<Expr>	bridge_ast_of<Expr>	(clang::Expr *  , clang_context &ctx);
template ptr<Stmt>	bridge_ast_of<Stmt>	(clang::Stmt *  , clang_context &ctx);
template ptr<Ctyp>	bridge_ast_of<Ctyp>	(clang::QualType, clang_context &ctx);
template ptr<TypeLoc>	bridge_ast_of<TypeLoc>	(clang::TypeLoc , clang_context &ctx);
