#ifndef CLANG_RANGES_H
#define CLANG_RANGES_H

#include <boost/range/iterator_range.hpp>

/********************************************************
 * <HACKS FOR CLANG INELEGANCES>
 * TODO: Remove when clang interfaces improve.
 ********************************************************/

template<typename DeclT>
auto
decl_range (DeclT const *D)
    -> decltype (boost::make_iterator_range (D->decls_begin (), // sometimes it's called decls
                                             D->decls_end ()))
{
  return boost::make_iterator_range (D->decls_begin (),
                                     D->decls_end ());
}


template<typename DeclT>
auto
decl_range (DeclT const *D)
    -> decltype (boost::make_iterator_range (D->decl_begin (), // and sometimes it's decl
                                             D->decl_end ()))
{
  return boost::make_iterator_range (D->decl_begin (),
                                     D->decl_end ());
}


static const auto
arg_range = [] (clang::CallExpr *S)
{
  return boost::make_iterator_range (S->getArgs (), // ExprIterator is completely useless
                                     S->getArgs () + S->getNumArgs ());
};


static const auto
designator_range = [] (clang::DesignatedInitExpr *S)
{
  return boost::make_iterator_range (S->designators_begin (),
                                     S->designators_end ());
};


static const auto
param_range = [] (clang::FunctionDecl *D)
{
  return boost::make_iterator_range (D->param_begin (),
                                     D->param_end ());
};


static const auto
arg_type_range = [] (clang::FunctionProtoType *T)
{
  return boost::make_iterator_range (T->param_type_begin (),
                                     T->param_type_end ());
};


static const auto
base_spec_range = [] (clang::CXXRecordDecl *D)
{
  return boost::make_iterator_range (D->bases_begin (),
                                     D->bases_end ());
};



struct offsetof_node_iterator
{
  typedef clang::OffsetOfExpr::OffsetOfNode *	iterator_type;
private:
  clang::OffsetOfExpr *S;
  unsigned component;

  typedef std::iterator_traits<iterator_type>	traits_type;

public:
  typedef traits_type::iterator_category	iterator_category;
  typedef traits_type::value_type		value_type;
  typedef traits_type::difference_type		difference_type;
  typedef traits_type::reference		reference;
  typedef traits_type::pointer			pointer;

public:
  offsetof_node_iterator (clang::OffsetOfExpr *S, unsigned component = 0)
    : S (S)
    , component (component)
  {
  }

  bool operator < (offsetof_node_iterator const &rhs) const
  {
    assert (S == rhs.S);
    return component < rhs.component;
  }

  bool operator != (offsetof_node_iterator const &rhs) const
  {
    assert (S == rhs.S);
    return component != rhs.component;
  }

  offsetof_node_iterator &operator++ ()
  {
    ++component;
    return *this;
  }

  clang::OffsetOfExpr::OffsetOfNode const &operator* () const
  {
    return S->getComponent (component);
  }
};


static const auto
offsetof_node_range = [] (clang::OffsetOfExpr *S)
{
  return boost::make_iterator_range (offsetof_node_iterator (S),
                                     offsetof_node_iterator (S, S->getNumComponents ()));
};


namespace clang
{
  static inline auto
  begin (TemplateParameterList *L)
    -> decltype (L->begin ())
  {
    return L->begin ();
  }

  static inline auto
  end (TemplateParameterList *L)
    -> decltype (L->end ())
  {
    return L->end ();
  }
}

/********************************************************
 * </HACKS FOR CLANG INELEGANCES>
 ********************************************************/

#endif /* CLANG_RANGES_H */
