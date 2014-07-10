#include "OCamlVisitor.h"


/****************************************************
 * {{{1 Types
 */


bool
OCamlVisitor::TraverseType (clang::QualType T)
{
  TRACE;

  Base::TraverseType (T);
  ptr<Ctyp_> unqual = stack.pop ();

  clang::Qualifiers quals = T.getLocalQualifiers ();

  std::vector<TypeQualifier> qualifiers;
  if (quals.hasConst ())	qualifiers.push_back (TQ_Const);
  if (quals.hasVolatile ())	qualifiers.push_back (TQ_Volatile);
  if (quals.hasRestrict ())	qualifiers.push_back (TQ_Restrict);

  switch (quals.getObjCGCAttr ())
    {
    case clang::Qualifiers::GCNone:
      // Nothing to add.
      break;
    case clang::Qualifiers::Weak:
      qualifiers.push_back (TQ_Weak);
      break;
    case clang::Qualifiers::Strong:
      qualifiers.push_back (TQ_Strong);
      break;
    }

  switch (quals.getObjCLifetime ())
    {
    case clang::Qualifiers::OCL_None:
      // Nothing to add.
      break;
    case clang::Qualifiers::OCL_ExplicitNone:
      qualifiers.push_back (TQ_OCL_ExplicitNone);
      break;
    case clang::Qualifiers::OCL_Strong:
      qualifiers.push_back (TQ_OCL_Strong);
      break;
    case clang::Qualifiers::OCL_Weak:
      qualifiers.push_back (TQ_OCL_Weak);
      break;
    case clang::Qualifiers::OCL_Autoreleasing:
      qualifiers.push_back (TQ_OCL_Autoreleasing);
      break;
    }

  option<int> addressSpace;
  if (quals.hasAddressSpace ())
    addressSpace = quals.getAddressSpace ();

  ptr<Ctyp> ctyp  = mkCtyp ();
  ctyp->t         = unqual;
  ctyp->t_cref    = ref (T);
  ctyp->t_qual    = qualifiers;
  ctyp->t_aspace  = addressSpace;
  ctyp->t_self    = ctyp;
  if (T.getCanonicalType () == T)
    ctyp->t_canon = ctyp;
  else
    ctyp->t_canon = must_traverse (T.getCanonicalType ());
  stack.push (ctyp);

  return true;
}


bool
OCamlVisitor::TraverseBuiltinType (clang::BuiltinType *T)
{
  TRACE;

  BuiltinType bt = translate_builtin_type (T->getKind ());

  stack.push (mkBuiltinType (bt));

  return true;
}



bool
OCamlVisitor::TraversePointerType (clang::PointerType *T)
{
  TRACE;

  ptr<Ctyp> pointee = must_traverse (T->getPointeeType ());

  stack.push (mkPointerType (pointee));

  return true;
}


bool
OCamlVisitor::TraverseFunctionProtoType (clang::FunctionProtoType *T)
{
  TRACE;

  ptr<Ctyp> result = must_traverse (T->getResultType ());
  list<Ctyp> args = traverse_list (arg_type_range (T));

  // TODO: exceptions

  stack.push (mkFunctionProtoType (result, args));

  return true;
}


bool
OCamlVisitor::TraverseFunctionNoProtoType (clang::FunctionNoProtoType *T)
{
  TRACE;

  ptr<Ctyp> result = must_traverse (T->getResultType ());

  stack.push (mkFunctionNoProtoType (result));

  return true;
}




#define UNIMP_TYPE(CLASS)						\
bool									\
OCamlVisitor::Traverse##CLASS##Type (clang::CLASS##Type *T)		\
{									\
  TODO;									\
  TRACE;								\
  Base::Traverse##CLASS##Type (T);					\
  stack.push (mk##CLASS##Type ());					\
  return true;								\
}


bool
OCamlVisitor::TraverseAttributedType (clang::AttributedType *T)
{
  TRACE;

  AttributedTypeKind attr_kind =
    translate_attributed_type_kind (T->getAttrKind ());

  ptr<Ctyp> modified_type = must_traverse (T->getModifiedType ());

  stack.push (mkAttributedType (attr_kind, modified_type));

  return true;
}


UNIMP_TYPE(Atomic)
UNIMP_TYPE(Auto)
UNIMP_TYPE(BlockPointer)


bool
OCamlVisitor::TraverseComplexType (clang::ComplexType *T)
{
  TRACE;

  ptr<Ctyp> element = must_traverse (T->getElementType ());

  stack.push (mkComplexType (element));

  return true;
}


bool
OCamlVisitor::TraverseVectorType (clang::VectorType *T)
{
  TRACE;

  ptr<Ctyp> elt_type = must_traverse (T->getElementType ());
  unsigned num_elts = T->getNumElements ();
  VectorKind vect_kind =
    translate_vector_kind (T->getVectorKind ());

  stack.push (mkVectorType (elt_type, num_elts, vect_kind));

  return true;
}


bool
OCamlVisitor::TraverseConstantArrayType (clang::ConstantArrayType *T)
{
  TRACE;

  ptr<Ctyp> element = must_traverse (T->getElementType ());
  uint64_t size = T->getSize ().getZExtValue ();

  stack.push (mkConstantArrayType (element, size));

  return true;
}


bool
OCamlVisitor::TraverseDecayedType (clang::DecayedType *T)
{
  TRACE;

  ptr<Ctyp> decayed = must_traverse (T->getDecayedType ());
  ptr<Ctyp> original = must_traverse (T->getOriginalType ());

  stack.push (mkDecayedType (decayed, original));

  return true;
}


bool
OCamlVisitor::TraverseDecltypeType (clang::DecltypeType *T)
{
  TRACE;

  ptr<Expr> expr = must_traverse (T->getUnderlyingExpr ());

  stack.push (mkDecltypeType (expr));

  return true;
}


UNIMP_TYPE(DependentName)
UNIMP_TYPE(DependentSizedArray)
UNIMP_TYPE(DependentSizedExtVector)
UNIMP_TYPE(DependentTemplateSpecialization)
bool
OCamlVisitor::TraverseElaboratedType (clang::ElaboratedType *T)
{
  TRACE;

  TraverseNestedNameSpecifier (T->getQualifier ());
  ptr<Ctyp> type = must_traverse (T->getNamedType ());

  stack.push (mkElaboratedType (type));

  return true;
}


bool
OCamlVisitor::TraverseEnumType (clang::EnumType *T)
{
  TRACE;

  clang::StringRef name = T->getDecl ()->getName ();

  stack.push (mkEnumType (name));

  return true;
}


bool
OCamlVisitor::TraverseExtVectorType (clang::ExtVectorType *T)
{
  TRACE;

  ptr<Ctyp> elt_type = must_traverse (T->getElementType ());
  unsigned num_elts = T->getNumElements ();
  VectorKind vect_kind =
    translate_vector_kind (T->getVectorKind ());

  stack.push (mkExtVectorType (elt_type, num_elts, vect_kind));

  return true;
}


bool
OCamlVisitor::TraverseIncompleteArrayType (clang::IncompleteArrayType *T)
{
  TRACE;

  ptr<Ctyp> element = must_traverse (T->getElementType ());

  stack.push (mkIncompleteArrayType (element));

  return true;
}


UNIMP_TYPE(InjectedClassName)
UNIMP_TYPE(LValueReference)
UNIMP_TYPE(MemberPointer)
UNIMP_TYPE(ObjCInterface)
UNIMP_TYPE(ObjCObjectPointer)
UNIMP_TYPE(ObjCObject)
UNIMP_TYPE(PackExpansion)
bool
OCamlVisitor::TraverseParenType (clang::ParenType *T)
{
  TRACE;

  ptr<Ctyp> inner = must_traverse (T->getInnerType ());

  stack.push (mkParenType (inner));

  return true;
}


bool
OCamlVisitor::TraverseRecordType (clang::RecordType *T)
{
  TRACE;

  TagTypeKind kind = translate_tag_type_kind (T->getDecl ()->getTagKind ());
  clang::StringRef name = T->getDecl ()->getName ();

  stack.push (mkRecordType (kind, name));

  return true;
}


UNIMP_TYPE(RValueReference)
UNIMP_TYPE(SubstTemplateTypeParmPack)
UNIMP_TYPE(SubstTemplateTypeParm)
UNIMP_TYPE(TemplateSpecialization)
bool
OCamlVisitor::TraverseTemplateTypeParmType (clang::TemplateTypeParmType *T)
{
  TRACE;

  option<clang::StringRef> name;
  if (T->getDecl ())
    name = T->getDecl ()->getName ();

  stack.push (mkTemplateTypeParmType (name));

  return true;
}


bool
OCamlVisitor::TraverseTypedefType (clang::TypedefType *T)
{
  TRACE;

  clang::StringRef name = T->getDecl ()->getName ();

  stack.push (mkTypedefType (name));

  return true;
}


bool
OCamlVisitor::TraverseTypeOfExprType (clang::TypeOfExprType *T)
{
  TRACE;

  ptr<Expr> expr = must_traverse (T->getUnderlyingExpr ());

  stack.push (mkTypeOfExprType (expr));

  return true;
}


bool
OCamlVisitor::TraverseTypeOfType (clang::TypeOfType *T)
{
  TRACE;

  ptr<Ctyp> type = must_traverse (T->getUnderlyingType ());

  stack.push (mkTypeOfType (type));

  return true;
}


UNIMP_TYPE(UnaryTransform)
UNIMP_TYPE(UnresolvedUsing)
bool
OCamlVisitor::TraverseVariableArrayType (clang::VariableArrayType *T)
{
  TRACE;

  ptr<Ctyp> element = must_traverse (T->getElementType ());
  option<Expr> size = maybe_traverse (T->getSizeExpr ());

  stack.push (mkVariableArrayType (element, size));

  return true;
}


// }}}
