#include "OCamlVisitor.h"


/****************************************************
 * {{{1 Declarations
 */

bool
OCamlVisitor::TraverseDecl (clang::Decl *D)
{
  Base::TraverseDecl (D);

  ptr<Decl> decl = mkDecl ();
  decl->d      = stack.pop ();
  decl->d_cref = ref (D);
  decl->d_sloc = sloc (D);
  stack.push (decl);

  return true;
}

bool
OCamlVisitor::TraverseFunctionDecl (clang::FunctionDecl *D)
{
  TRACE;

  // TODO: what are these? probably irrelevant in C.
  TraverseNestedNameSpecifierLoc (D->getQualifierLoc ());
  TraverseDeclarationNameInfo (D->getNameInfo ());

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
  clang::StringRef name = getName (D);

  stack.push (mkFunctionDecl (type, name, body));

  return true;
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

  // Some members may be implicit (from inline unions).
  list<Decl> members = traverse_explicit_decls (D);
  clang::StringRef name = D->getName ();

  stack.push (mkRecordDecl (name, members));

  return true;
}


bool
OCamlVisitor::TraverseFieldDecl (clang::FieldDecl *D)
{
  TRACE;

  ptr<Tloc> type = getTypeLoc (D);

  option<Expr> bitwidth;
  if (D->isBitField ())
    bitwidth = must_traverse (D->getBitWidth ());

  option<Expr> init;
  if (D->hasInClassInitializer ())
    init = must_traverse (D->getInClassInitializer ());

  clang::StringRef name = D->getName ();

  stack.push (mkFieldDecl (type, name, bitwidth, init));

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
    stack.push (mkUnimpDecl (#CLASS));				\
    return true;						\
  }


UNIMP_DECL (AccessSpecDecl)
UNIMP_DECL (BlockDecl)
UNIMP_DECL (CapturedDecl)
UNIMP_DECL (ClassScopeFunctionSpecializationDecl)
UNIMP_DECL (ClassTemplateDecl)
UNIMP_DECL (FileScopeAsmDecl)
UNIMP_DECL (FriendDecl)
UNIMP_DECL (FriendTemplateDecl)
UNIMP_DECL (FunctionTemplateDecl)
UNIMP_DECL (ImportDecl)
UNIMP_DECL (IndirectFieldDecl)
UNIMP_DECL (LabelDecl)
UNIMP_DECL (LinkageSpecDecl)
UNIMP_DECL (MSPropertyDecl)
UNIMP_DECL (NamespaceAliasDecl)
UNIMP_DECL (NamespaceDecl)
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
UNIMP_DECL (TemplateTypeParmDecl)
UNIMP_DECL (TypeAliasDecl)
UNIMP_DECL (TypeAliasTemplateDecl)
UNIMP_DECL (UnresolvedUsingTypenameDecl)
UNIMP_DECL (UnresolvedUsingValueDecl)
UNIMP_DECL (UsingDecl)
UNIMP_DECL (UsingDirectiveDecl)
UNIMP_DECL (UsingShadowDecl)

// }}}
