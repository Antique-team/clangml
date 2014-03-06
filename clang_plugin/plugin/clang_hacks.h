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
  return boost::make_iterator_range (T->arg_type_begin (),
                                     T->arg_type_end ());
};

/********************************************************
 * </HACKS FOR CLANG INELEGANCES>
 ********************************************************/
