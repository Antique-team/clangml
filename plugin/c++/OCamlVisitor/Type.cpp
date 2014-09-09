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

  return stack.push (ctyp);
}


bool
OCamlVisitor::TraverseBuiltinType (clang::BuiltinType *T)
{
  TRACE;

  BuiltinType bt = translate_builtin_type (T->getKind ());

  return stack.push (mkBuiltinType (bt));
}



bool
OCamlVisitor::TraversePointerType (clang::PointerType *T)
{
  TRACE;

  ptr<Ctyp> pointee = must_traverse (T->getPointeeType ());

  return stack.push (mkPointerType (pointee));
}


bool
OCamlVisitor::TraverseFunctionProtoType (clang::FunctionProtoType *T)
{
  TRACE;

  ptr<Ctyp> result = must_traverse (T->getResultType ());
  list<Ctyp> args = traverse_list (arg_type_range (T));

  // TODO: exceptions

  return stack.push (mkFunctionProtoType (result, args));
}


bool
OCamlVisitor::TraverseFunctionNoProtoType (clang::FunctionNoProtoType *T)
{
  TRACE;

  ptr<Ctyp> result = must_traverse (T->getResultType ());

  return stack.push (mkFunctionNoProtoType (result));
}




#define UNIMP_TYPE(CLASS)						\
bool									\
OCamlVisitor::Traverse##CLASS##Type (clang::CLASS##Type *T)		\
{									\
  TODO;									\
  TRACE;								\
  Base::Traverse##CLASS##Type (T);					\
                                                                        \
  return stack.push (mk##CLASS##Type ());				\
}


bool
OCamlVisitor::TraverseAttributedType (clang::AttributedType *T)
{
  TRACE;

  AttributedTypeKind attr_kind =
    translate_attributed_type_kind (T->getAttrKind ());

  ptr<Ctyp> modified_type = must_traverse (T->getModifiedType ());

  return stack.push (mkAttributedType (attr_kind, modified_type));
}


UNIMP_TYPE(Auto)
UNIMP_TYPE(BlockPointer)


bool
OCamlVisitor::TraverseComplexType (clang::ComplexType *T)
{
  TRACE;

  ptr<Ctyp> element = must_traverse (T->getElementType ());

  return stack.push (mkComplexType (element));
}


bool
OCamlVisitor::TraverseAtomicType (clang::AtomicType *T)
{
  TRACE;

  ptr<Ctyp> value = must_traverse (T->getValueType ());

  return stack.push (mkAtomicType (value));
}


bool
OCamlVisitor::TraverseVectorType (clang::VectorType *T)
{
  TRACE;

  ptr<Ctyp> elt_type = must_traverse (T->getElementType ());
  unsigned num_elts = T->getNumElements ();
  VectorKind vect_kind =
    translate_vector_kind (T->getVectorKind ());

  return stack.push (mkVectorType (elt_type, num_elts, vect_kind));
}


bool
OCamlVisitor::TraverseConstantArrayType (clang::ConstantArrayType *T)
{
  TRACE;

  ptr<Ctyp> element = must_traverse (T->getElementType ());
  uint64_t size = T->getSize ().getZExtValue ();

  return stack.push (mkConstantArrayType (element, size));
}


bool
OCamlVisitor::TraverseDecayedType (clang::DecayedType *T)
{
  TRACE;

  ptr<Ctyp> decayed = must_traverse (T->getDecayedType ());
  ptr<Ctyp> original = must_traverse (T->getOriginalType ());

  return stack.push (mkDecayedType (decayed, original));
}


bool
OCamlVisitor::TraverseDecltypeType (clang::DecltypeType *T)
{
  TRACE;

  ptr<Expr> expr = must_traverse (T->getUnderlyingExpr ());

  return stack.push (mkDecltypeType (expr));
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

  return stack.push (mkElaboratedType (type));
}


bool
OCamlVisitor::TraverseEnumType (clang::EnumType *T)
{
  TRACE;

  clang::StringRef name = T->getDecl ()->getName ();

  return stack.push (mkEnumType (name));
}


bool
OCamlVisitor::TraverseExtVectorType (clang::ExtVectorType *T)
{
  TRACE;

  ptr<Ctyp> elt_type = must_traverse (T->getElementType ());
  unsigned num_elts = T->getNumElements ();
  VectorKind vect_kind =
    translate_vector_kind (T->getVectorKind ());

  return stack.push (mkExtVectorType (elt_type, num_elts, vect_kind));
}


bool
OCamlVisitor::TraverseIncompleteArrayType (clang::IncompleteArrayType *T)
{
  TRACE;

  ptr<Ctyp> element = must_traverse (T->getElementType ());

  return stack.push (mkIncompleteArrayType (element));
}

bool
OCamlVisitor::TraverseObjCObjectPointerType (clang::ObjCObjectPointerType *T)
{
  TRACE;

  ptr<Ctyp> pointee = must_traverse (T->getPointeeType ());

  return stack.push (mkObjCObjectPointerType (pointee));
}

bool
OCamlVisitor::TraverseObjCObjectType (clang::ObjCObjectType *T)
{
  TRACE;

  ptr<Ctyp> base = must_traverse (T->getBaseType ());

  return stack.push (mkObjCObjectType (base));
}

bool
OCamlVisitor::TraverseObjCInterfaceType (clang::ObjCInterfaceType *T)
{
  TRACE;

  clang::StringRef name = T->getDecl ()->getName ();

  return stack.push (mkObjCInterfaceType (name));
}


UNIMP_TYPE(InjectedClassName)
UNIMP_TYPE(LValueReference)
UNIMP_TYPE(MemberPointer)
UNIMP_TYPE(PackExpansion)
bool
OCamlVisitor::TraverseParenType (clang::ParenType *T)
{
  TRACE;

  ptr<Ctyp> inner = must_traverse (T->getInnerType ());

  return stack.push (mkParenType (inner));
}


bool
OCamlVisitor::TraverseRecordType (clang::RecordType *T)
{
  TRACE;

  TagTypeKind kind = translate_tag_type_kind (T->getDecl ()->getTagKind ());
  clang::StringRef name = T->getDecl ()->getName ();

  return stack.push (mkRecordType (kind, name));
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

  return stack.push (mkTemplateTypeParmType (name));
}


bool
OCamlVisitor::TraverseTypedefType (clang::TypedefType *T)
{
  TRACE;

  clang::StringRef name = T->getDecl ()->getName ();

  return stack.push (mkTypedefType (name));
}


bool
OCamlVisitor::TraverseTypeOfExprType (clang::TypeOfExprType *T)
{
  TRACE;

  ptr<Expr> expr = must_traverse (T->getUnderlyingExpr ());

  return stack.push (mkTypeOfExprType (expr));
}


bool
OCamlVisitor::TraverseTypeOfType (clang::TypeOfType *T)
{
  TRACE;

  ptr<Ctyp> type = must_traverse (T->getUnderlyingType ());

  return stack.push (mkTypeOfType (type));
}


UNIMP_TYPE(UnaryTransform)
UNIMP_TYPE(UnresolvedUsing)
bool
OCamlVisitor::TraverseVariableArrayType (clang::VariableArrayType *T)
{
  TRACE;

  ptr<Ctyp> element = must_traverse (T->getElementType ());
  option<Expr> size = maybe_traverse (T->getSizeExpr ());

  return stack.push (mkVariableArrayType (element, size));
}


// }}}
