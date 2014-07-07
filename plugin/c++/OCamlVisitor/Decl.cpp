#include "OCamlVisitor.h"


ptr<DeclarationName>
OCamlVisitor::translate_declaration_name (clang::DeclarationName const &name)
{
  switch (name.getNameKind ())
    {
    case clang::DeclarationName::Identifier:
      return mkDN_Identifier (strdup (name.getAsString ().c_str ()));
    case clang::DeclarationName::ObjCZeroArgSelector:
      printf ("ObjCZeroArgSelector: %s\n", name.getAsString ().c_str ());
      break;
    case clang::DeclarationName::ObjCOneArgSelector:
      printf ("ObjCOneArgSelector: %s\n", name.getAsString ().c_str ());
      break;
    case clang::DeclarationName::ObjCMultiArgSelector:
      printf ("ObjCMultiArgSelector: %s\n", name.getAsString ().c_str ());
      break;
    case clang::DeclarationName::CXXConstructorName:
      return mkDN_CXXConstructorName (must_traverse (name.getCXXNameType ()));
    case clang::DeclarationName::CXXDestructorName:
      return mkDN_CXXDestructorName (must_traverse (name.getCXXNameType ()));
    case clang::DeclarationName::CXXConversionFunctionName:
      return mkDN_CXXConversionFunctionName ();
    case clang::DeclarationName::CXXOperatorName:
      return mkDN_CXXOperatorName (translate_overloaded_operator_kind (name.getCXXOverloadedOperator ()));
    case clang::DeclarationName::CXXLiteralOperatorName:
      printf ("CXXLiteralOperatorName: %s\n", name.getAsString ().c_str ());
      break;
    case clang::DeclarationName::CXXUsingDirective:
      printf ("CXXUsingDirective: %s\n", name.getAsString ().c_str ());
      break;
    }

  return mkDN_Identifier ("<invalid>");
}


/****************************************************
 * {{{1 Declarations
 */

bool
OCamlVisitor::TraverseDecl (clang::Decl *D)
{
  Base::TraverseDecl (D);

  ptr<Decl> decl = mkDecl ();
  decl->d        = stack.pop ();
  decl->d_cref   = ref (D);
  decl->d_sloc   = sloc (D);
  stack.push (decl);

  return true;
}


bool
OCamlVisitor::TraverseFunctionDecl (clang::FunctionDecl *D)
{
  TRACE;

  // TODO: what are these? probably irrelevant in C.
  //TraverseNestedNameSpecifierLoc (D->getQualifierLoc ());
  //TraverseDeclarationNameInfo (D->getNameInfo ());

  // Function type, including parameters.
  ptr<Tloc> type;
  if (D->getTypeSourceInfo ())
    type = getTypeLoc (D);
  else
    {
      // TODO: implement this

      // Built-in implicit function declarations such as
      // printf don't have a valid TSI.
      //ptr<Tloc> result = mkBuiltinTypeLoc (sloc (D), BT_Void);
      //list<Decl> args;// = traverse_list (param_range (D));

      // TODO: exceptions

      //type = mkFunctionProtoTypeLoc (sloc (D), result, args);

      throw std::runtime_error ("unsupported built-in function declaration");
    }

  // Function body, or None.
  option<Stmt> body;
  if (D->isThisDeclarationADefinition ())
    body = must_traverse (D->getBody ());

  // TODO: Constructor initialisers.

  // Function name.
  ptr<DeclarationName> name = translate_declaration_name (D->getDeclName ());

  stack.push (mkFunctionDecl (type, name, body));

  return true;
}


bool
OCamlVisitor::TraverseCXXMethodDecl (clang::CXXMethodDecl *D)
{
  TRACE;

  return TraverseFunctionDecl (D);
}


bool
OCamlVisitor::TraverseCXXConstructorDecl (clang::CXXConstructorDecl *D)
{
  TRACE;

  return TraverseCXXMethodDecl (D);
}


bool
OCamlVisitor::TraverseCXXDestructorDecl (clang::CXXDestructorDecl *D)
{
  TRACE;

  return TraverseCXXMethodDecl (D);
}


bool
OCamlVisitor::TraverseCXXConversionDecl (clang::CXXConversionDecl *D)
{
  TRACE;

  return TraverseCXXMethodDecl (D);
}


bool
OCamlVisitor::TraverseEmptyDecl (clang::EmptyDecl *D)
{
  TRACE;

  stack.push (mkEmptyDecl ());

  return true;
}


bool
OCamlVisitor::TraverseTypedefDecl (clang::TypedefDecl *D)
{
  TRACE;

  ptr<Tloc> type = getTypeLoc (D);
  clang::StringRef name = D->getName ();

  stack.push (mkTypedefDecl (type, name));

  return true;
}


bool
OCamlVisitor::TraverseRecordDecl (clang::RecordDecl *D)
{
  TRACE;

  TagTypeKind kind = translate_tag_type_kind (D->getTagKind ());
  option<list<Decl>> members;
  if (D->isCompleteDefinition ())
    // Some members may be implicit (from inline unions).
    members = implicit_cast<list<Decl>> (traverse_explicit_decls (D));
  clang::StringRef name = D->getName ();
  list<CxxBaseSpecifier> bases;

  stack.push (mkRecordDecl (kind, name, members, bases));

  return true;
}


bool
OCamlVisitor::TraverseCXXBaseSpecifier (clang::CXXBaseSpecifier const &B)
{
  TRACE;

  ptr<CxxBaseSpecifier> base = mkCxxBaseSpecifier ();
  base->cbs_virtual          = B.isVirtual ();
  base->cbs_base_of_class    = B.isBaseOfClass ();
  base->cbs_pack_expansion   = B.isPackExpansion ();
  base->cbs_inherit_ctors    = B.getInheritConstructors ();
  base->cbs_access_spec      = translate_access_specifier (B.getAccessSpecifier ());
  base->cbs_type             = must_traverse (B.getTypeSourceInfo ());

  stack.push (base);

  return true;
}


bool
OCamlVisitor::TraverseCXXRecordDecl (clang::CXXRecordDecl *D)
{
  TRACE;

  TagTypeKind kind = translate_tag_type_kind (D->getTagKind ());
  // Some members may be implicit (from inline unions).
  list<Decl> members = traverse_explicit_decls (D);
  clang::StringRef name = D->getName ();
  list<CxxBaseSpecifier> bases;
  if (D->isCompleteDefinition ())
    bases = traverse_list (base_spec_range (D));

  stack.push (mkRecordDecl (kind, name, members, bases));

  return true;
}


bool
OCamlVisitor::TraverseFieldDecl (clang::FieldDecl *D)
{
  TRACE;

  ptr<FieldDecl> field = mkFieldDecl ();

  field->fd_type = getTypeLoc (D);
  field->fd_name = D->getName ();
  if (D->isBitField ())
    field->fd_bitw = must_traverse (D->getBitWidth ());
  if (D->hasInClassInitializer ())
    field->fd_init = must_traverse (D->getInClassInitializer ());
  field->fd_index = D->getFieldIndex ();
  field->fd_mutable = D->isMutable ();

  stack.push (mkFieldDecl (field));

  return true;
}


bool
OCamlVisitor::TraverseEnumDecl (clang::EnumDecl *D)
{
  TRACE;

  clang::StringRef name = D->getName ();
  list<Decl> enumerators = traverse_list (decl_range (D));

  stack.push (mkEnumDecl (name, enumerators));

  return true;
}


bool
OCamlVisitor::TraverseEnumConstantDecl (clang::EnumConstantDecl *D)
{
  TRACE;

  clang::StringRef name = D->getName ();
  option<Expr> init = maybe_traverse (D->getInitExpr ());

  stack.push (mkEnumConstantDecl (name, init));

  return true;
}


bool
OCamlVisitor::TraverseParmVarDecl (clang::ParmVarDecl *D)
{
  TRACE;

  TraverseNestedNameSpecifierLoc (D->getQualifierLoc ());

  ptr<Tloc> type = getTypeLoc (D);
  clang::StringRef name = D->getName ();

  stack.push (mkParmVarDecl (type, name));

  return true;
}


bool
OCamlVisitor::TraverseVarDecl (clang::VarDecl *D)
{
  TRACE;

  TraverseNestedNameSpecifierLoc (D->getQualifierLoc ());

  ptr<Tloc> type = getTypeLoc (D);
  clang::StringRef name = D->getName ();
  option<Expr> init = maybe_traverse (D->getInit ());

  stack.push (mkVarDecl (type, name, init));

  return true;
}


bool
OCamlVisitor::TraverseTranslationUnitDecl (clang::TranslationUnitDecl *D)
{
  TRACE;

  // We filter out implicit declarations before iterating.
  list<Decl> decls = traverse_explicit_decls (D);

  stack.push (mkTranslationUnitDecl (decls));

  return true;
}

#define UNIMP_DECL(CLASS)					\
  bool OCamlVisitor::Traverse##CLASS (clang::CLASS *D)		\
  {								\
    TODO;							\
    TRACE;							\
    IGNORE_ADT (CLASS, D);					\
    stack.push (mk##CLASS ());					\
    return true;						\
  }


bool
OCamlVisitor::TraverseAccessSpecDecl (clang::AccessSpecDecl *D)
{
  TRACE;

  AccessSpecifier spec = translate_access_specifier (D->getAccess ());

  stack.push (mkAccessSpecDecl (spec));

  return true;
}


UNIMP_DECL (BlockDecl)
UNIMP_DECL (CapturedDecl)
UNIMP_DECL (ClassScopeFunctionSpecializationDecl)


bool
OCamlVisitor::TraverseClassTemplateDecl (clang::ClassTemplateDecl *D)
{
  TRACE;

  ptr<Decl> templated = must_traverse (D->getTemplatedDecl ());
  list<Decl> params = traverse_list (D->getTemplateParameters ());

  stack.push (mkClassTemplateDecl (templated, params));

  return true;
}


bool
OCamlVisitor::TraverseFileScopeAsmDecl (clang::FileScopeAsmDecl *D)
{
  TRACE;

  ptr<Expr> asmString = must_traverse (D->getAsmString ());

  stack.push (mkFileScopeAsmDecl (asmString));

  return true;
}


UNIMP_DECL (FriendDecl)
UNIMP_DECL (FriendTemplateDecl)
UNIMP_DECL (FunctionTemplateDecl)
UNIMP_DECL (ImportDecl)
UNIMP_DECL (IndirectFieldDecl)
UNIMP_DECL (LabelDecl)


bool
OCamlVisitor::TraverseLinkageSpecDecl (clang::LinkageSpecDecl *D)
{
  TRACE;

  // We filter out implicit declarations before iterating.
  list<Decl> decls = traverse_explicit_decls (D);
  Language lang = Lang_C;
  switch (D->getLanguage ())
    {
    case clang::LinkageSpecDecl::lang_c:
      lang = Lang_C;
      break;
    case clang::LinkageSpecDecl::lang_cxx:
      lang = Lang_CXX;
      break;
    }

  stack.push (mkLinkageSpecDecl (decls, lang));

  return true;
}
UNIMP_DECL (MSPropertyDecl)
UNIMP_DECL (NamespaceAliasDecl)


bool
OCamlVisitor::TraverseNamespaceDecl (clang::NamespaceDecl *D)
{
  TRACE;

  // We filter out implicit declarations before iterating.
  list<Decl> decls = traverse_explicit_decls (D);
  clang::StringRef name = D->getName ();
  bool isInline = D->isInline ();

  stack.push (mkNamespaceDecl (name, isInline, decls));

  return true;
}
UNIMP_DECL (NonTypeTemplateParmDecl)
UNIMP_DECL (ObjCCategoryDecl)
UNIMP_DECL (ObjCCategoryImplDecl)
UNIMP_DECL (ObjCCompatibleAliasDecl)
UNIMP_DECL (ObjCImplementationDecl)
UNIMP_DECL (ObjCInterfaceDecl)
UNIMP_DECL (ObjCMethodDecl)
UNIMP_DECL (ObjCPropertyDecl)
UNIMP_DECL (ObjCPropertyImplDecl)
UNIMP_DECL (ObjCProtocolDecl)
UNIMP_DECL (OMPThreadPrivateDecl)
UNIMP_DECL (StaticAssertDecl)
UNIMP_DECL (TemplateTemplateParmDecl)


bool
OCamlVisitor::TraverseTemplateTypeParmDecl (clang::TemplateTypeParmDecl *D)
{
  TRACE;

  // We filter out implicit declarations before iterating.
  ptr<Ctyp> type = must_traverse (clang::QualType (D->getTypeForDecl (), 0));
  option<Tloc> defaultArg = maybe_traverse (D->getDefaultArgumentInfo ());

  stack.push (mkTemplateTypeParmDecl (type, defaultArg));

  return true;
}
UNIMP_DECL (TypeAliasDecl)
UNIMP_DECL (TypeAliasTemplateDecl)
UNIMP_DECL (UnresolvedUsingTypenameDecl)
UNIMP_DECL (UnresolvedUsingValueDecl)


bool
OCamlVisitor::TraverseUsingDecl (clang::UsingDecl *D)
{
  TRACE;

  ptr<DeclarationName> dname = translate_declaration_name (D->getDeclName ());

  stack.push (mkUsingDecl (dname));

  return true;
}
UNIMP_DECL (UsingDirectiveDecl)
UNIMP_DECL (UsingShadowDecl)
UNIMP_DECL (VarTemplateDecl)
UNIMP_DECL (ClassTemplateSpecializationDecl)
UNIMP_DECL (ClassTemplatePartialSpecializationDecl)
UNIMP_DECL (ObjCAtDefsFieldDecl)
UNIMP_DECL (ObjCIvarDecl)
UNIMP_DECL (ImplicitParamDecl)
UNIMP_DECL (VarTemplateSpecializationDecl)
UNIMP_DECL (VarTemplatePartialSpecializationDecl)

// }}}
