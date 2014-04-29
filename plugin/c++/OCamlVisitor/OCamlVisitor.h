#ifndef OCAML_VISITOR_H
#define OCAML_VISITOR_H

#include "clang_context.h"
#include "clang_enums.h"
#include "dynamic_stack.h"
#include "delayed_exit.h"
#include "trace.h"

#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/Basic/SourceManager.h>

#include <sstream>

#include <boost/range/adaptor/filtered.hpp>

#include "clang_ranges.h"

using namespace ast_bridge;

#define TODO std::printf ("TODO: %s\n", __func__); delayed_exit E { 1 }

//#undef TRACE
//#define TRACE


struct OCamlVisitor
  : clang::RecursiveASTVisitor<OCamlVisitor>
{
  // Enable caching (and sharing) of bridge AST objects.
  bool sharing = true;
  // Enable sharing for Type (and Ctyp) objects.
  bool share_types = true;
  // Also enable sharing for TypeLoc objects, ignoring source
  // locations, thus making source locations for TypeLocs useless.
  bool share_type_locs = false;

  // Resolve all source locations.
  bool with_sloc = true;

  // Not static, so ID assignment works correctly.
  ptr<Sloc> const empty_sloc = mkSloc ();

private:
  typedef clang::RecursiveASTVisitor<OCamlVisitor> Base;

  clang_context &ctx;
  dynamic_stack stack;

  /****************************************************
   * {{{1 Traversal dispatch functions
   */

  adt_ptr cached (clang::QualType			p, adt_ptr value = nullptr);
  adt_ptr cached (clang::TypeLoc			p, adt_ptr value = nullptr);
  adt_ptr cached (clang::DesignatedInitExpr::Designator	p, adt_ptr value = nullptr);
  adt_ptr cached (clang::Decl *				p, adt_ptr value = nullptr);
  adt_ptr cached (clang::Stmt *				p, adt_ptr value = nullptr);
  adt_ptr cached (clang::CXXBaseSpecifier		p, adt_ptr value = nullptr);


  template<typename T>
  static void dump (T p)
  { p->dump (); }

  static void dump (clang::TypeLoc TL);
  static void dump (clang::DesignatedInitExpr::Designator const &p);
  static void dump (clang::CXXBaseSpecifier const &p);


  template<typename T>
  static void check_size (T p, size_t size_before, size_t size_after)
  {
    // Require stack to increase by exactly 1 (one value was pushed).
    if (size_after != size_before + 1)
      {
        dump (p);
        std::stringstream message;
        message << "Traversal function postcondition not held: "
                << "must create exactly 1 value, but created "
                << size_after - size_before;
        throw std::runtime_error (message.str ());
      }
  }

  // Traverse a clang object, but keep the result (if any) on
  // the stack. Requires that if p is a pointer or TypeLoc, it
  // is not null.
  template<typename T, bool (OCamlVisitor::*Fun) (T p)>
  void traverse (T p)
  {
    adt_ptr result = cached (p);
    if (result)
      stack.push (result);
    else
      {
        size_t size_before = stack.size ();
        (this->*Fun) (p);
        size_t size_after = stack.size ();

        check_size (p, size_before, size_after);

        cached (p, stack.top ());
      }
  }

  // TODO: unify this with the above
  template<typename T, typename A1, bool (OCamlVisitor::*Fun) (T p, A1 a1)>
  void traverse (T p, A1 a1)
  {
    adt_ptr result = cached (p);
    if (result)
      stack.push (result);
    else
      {
        size_t size_before = stack.size ();
        (this->*Fun) (p, a1);
        size_t size_after = stack.size ();

        check_size (p, size_before, size_after);

        cached (p, stack.top ());
      }
  }

  // These functions require the clang pointer to be non-null.
  // In case of TypeLoc, require the contained Type pointer
  // to be non-null.

  void traverse (clang::Decl *D);
  void traverse (clang::Stmt *S);
  void traverse (clang::TypeSourceInfo *TSI);
  void traverse (clang::TypeLoc TL);
  void traverse (clang::QualType T);
  void traverse (clang::DesignatedInitExpr::Designator const &D,
                 clang::DesignatedInitExpr *S);
  void traverse (clang::CXXBaseSpecifier const &B);


  // May take a pointer or an object.
  // This function and maybe_traverse both pop and return the
  // translation result (OCaml bridge object).
  template<typename T>
  dynamic_stack::element must_traverse (T p)
  {
    // Traverse, which does not pop.
    traverse (p);
    // Now, get the bridge pointer.
    dynamic_stack::element result = stack.pop ();
    // Require OCaml bridge pointer to be non-null.
    assert (result);
    return result;
  }

  // Return null if p is null, otherwise call an appropriate traversal
  // function and return its result.
  template<typename T>
  dynamic_stack::element maybe_traverse (T p)
  {
    if (!p)
      return dynamic_stack::element { };
    return must_traverse (p);
  }


  template<typename Range, typename... Args>
  dynamic_stack::range traverse_list (Range const &range, Args... args)
  {
    stack.push_mark ();
    for (auto child : range)
      traverse (child, args...);
    return stack.pop_marked ();
  }

  template<typename DeclT>
  dynamic_stack::range traverse_explicit_decls (DeclT *D)
  {
    using namespace boost::adaptors;

    return traverse_list (
             decl_range (D)
             | filtered ([] (clang::Decl *D) {
                 return !D->isImplicit ();
               })
           );
  }


  // }}}


  /****************************************************
   * {{{1 Common helpers
   */

  static clang::StringRef getName (clang::DeclRefExpr const *D)
  {
    clang::IdentifierInfo *info = D->getNameInfo ().getName ().getAsIdentifierInfo ();
    assert (info);
    return info->getName ();
  }

  // TypedefDecl and DeclaratorDecl
  template<typename DeclT>
  ptr<Tloc> getTypeLoc (DeclT const *D)
  {
    return must_traverse (D->getTypeSourceInfo ());
  }

  template<typename T>
  T const &deref (T *p) { return *p; }

  template<typename T>
  T const &deref (T const &p) { return p; }

  template<typename T>
  ptr<Sloc> sloc (T p)
  {
    if (!with_sloc)
      return empty_sloc;

    clang::SourceLocation start = deref (p).getLocStart ();
    clang::SourceLocation end   = deref (p).getLocEnd   ();

    assert (start.isValid () == end.isValid ());

    if (start.isInvalid ())
      return empty_sloc;

    ptr<Sloc> sloc = mkSloc ();

    sloc->loc_s = start;
    sloc->loc_e = end;

    return sloc;
  }

  // }}}


  /****************************************************
   * {{{1 Clang reference management
   */

  clang_ref<Decl> ref (clang::Decl *D);
  clang_ref<Stmt> ref (clang::Stmt *S);
  clang_ref<Expr> ref (clang::Expr *E);
  clang_ref<Ctyp> ref (clang::QualType T);
  clang_ref<Tloc> ref (clang::TypeLoc TL);


  // }}}

public:
  OCamlVisitor (clang_context &ctx);


  /****************************************************
   * {{{1 Visitor settings
   */

  //bool shouldVisitImplicitCode () const { return true; }

  // }}}

  /****************************************************
   * {{{1 Statements
   */
  bool TraverseDesignator (clang::DesignatedInitExpr::Designator D,
                           clang::DesignatedInitExpr *S);

  template<typename TypeFactory, typename ExprFactory>
  void pushUnaryExprOrTypeTraitExpr (clang::UnaryExprOrTypeTraitExpr *S,
                                     TypeFactory mkOfType,
                                     ExprFactory mkOfExpr);

  bool TraverseStmt (clang::Stmt *S);

#define ABSTRACT_STMT(STMT)
#define STMT(CLASS, BASE)	bool Traverse##CLASS (clang::CLASS *T);
#define EXPR(CLASS, BASE)	bool Traverse##CLASS (clang::CLASS *T);
#include <clang/AST/StmtNodes.inc>

  // }}}

  /****************************************************
   * {{{1 Types
   */

  bool TraverseType (clang::QualType T);

#define ABSTRACT_TYPE(CLASS, BASE)
#define TYPE(CLASS, BASE)	bool Traverse##CLASS##Type (clang::CLASS##Type *T);
#include <clang/AST/TypeNodes.def>

  // }}}

  /****************************************************
   * {{{1 TypeLocs
   */

  bool TraverseTypeLoc (clang::TypeLoc TL);

#define ABSTRACT_TYPELOC(CLASS, BASE)
#define TYPELOC(CLASS, BASE)	bool Traverse##CLASS##TypeLoc (clang::CLASS##TypeLoc TL);
#include <clang/AST/TypeLocNodes.def>

  // }}}

  /****************************************************
   * {{{1 Declarations
   */

  bool TraverseDecl (clang::Decl *D);

#define FUNCTION(CLASS, BASE)
  bool TraverseFunctionDecl (clang::FunctionDecl *D);

#define EMPTY(CLASS, BASE)
  bool TraverseEmptyDecl (clang::EmptyDecl *D);

#define TYPEDEF(CLASS, BASE)
  bool TraverseTypedefDecl (clang::TypedefDecl *D);

#define RECORD(CLASS, BASE)
  bool TraverseRecordDecl (clang::RecordDecl *D);

#define CXXRECORD(CLASS, BASE)
  bool TraverseCXXRecordDecl (clang::CXXRecordDecl *D);
  bool TraverseCXXBaseSpecifier (clang::CXXBaseSpecifier const &B);

#define FIELD(CLASS, BASE)
  bool TraverseFieldDecl (clang::FieldDecl *D);

#define ENUM(CLASS, BASE)
  bool TraverseEnumDecl (clang::EnumDecl *D);

#define ENUMCONSTANT(CLASS, BASE)
  bool TraverseEnumConstantDecl (clang::EnumConstantDecl *D);

#define PARMVAR(CLASS, BASE)
  bool TraverseParmVarDecl (clang::ParmVarDecl *D);

#define VAR(CLASS, BASE)
  bool TraverseVarDecl (clang::VarDecl *D);

#define TRANSLATIONUNIT(CLASS, BASE)
  bool TraverseTranslationUnitDecl (clang::TranslationUnitDecl *D);

#define ABSTRACT_DECL(DECL)
#define DECL(CLASS, BASE)					\
  bool Traverse##CLASS##Decl (clang::CLASS##Decl *D);
#include <clang/AST/DeclNodes.inc>

  // }}}

  /****************************************************
   * Final result
   */

  template<typename T>
  ptr<typename bridge_type<T>::type>
  translate (T Node)
  {
    assert (stack.empty ());
    return must_traverse (Node);
  }
};


#define IGNORE_ADT(CLASS, VAR)				\
  stack.push_mark ();					\
  Base::Traverse##CLASS (VAR);				\
  /* Drop everything made by previous calls. */		\
  size_t marker = stack.pop_mark ();			\
  while (marker--) stack.pop ()


#define UNIMP_STMT(TYPE, CLASS)				\
bool							\
OCamlVisitor::Traverse##CLASS (clang::CLASS *S)		\
{							\
  TODO;							\
  TRACE;						\
  IGNORE_ADT (CLASS, S);				\
  stack.push (mk##CLASS ());				\
  return true;						\
}


#endif /* OCAML_VISITOR_H */
