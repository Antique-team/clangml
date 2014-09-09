#include "OCamlVisitor.h"


/****************************************************
 * {{{1 TypeLocs
 */

bool
OCamlVisitor::TraverseTypeLoc (clang::TypeLoc TL)
{
#if 0
  TL.getType ().dump ();
  printf ("size = %u, align = %u\n",
          clang::TypeLoc::getFullDataSizeForType   (TL.getType ()),
          clang::TypeLoc::getLocalAlignmentForType (TL.getType ()));
#endif

  stack.push_mark ();
  Base::TraverseTypeLoc (TL);
  size_t marker = stack.pop_mark ();

  if (marker == 0)
    {
      printf ("WARNING: %s creates dummy TypeLoc, "
              "as derived function did not produce any\n",
              __func__);
      stack.push (mkBuiltinTypeLoc (BT_Void));
    }
  else if (marker > 1)
    {
      ptr<Tloc> mostRecent = stack.pop ();
      printf ("WARNING: %s drops all but most recent (out of %zu) TypeLoc\n",
              __func__, marker);
      // Keep the last one
      while (--marker) stack.pop ();
      stack.push (mostRecent);
    }

  // Amend with source locations.
  ptr<Tloc> type_loc = mkTloc ();
  type_loc->tl      = stack.pop ();
  type_loc->tl_cref = ref (TL);
  type_loc->tl_sloc = sloc (TL);
  type_loc->tl_type = must_traverse (TL.getType ());

  return stack.push (type_loc);
}


bool
OCamlVisitor::TraverseBuiltinTypeLoc (clang::BuiltinTypeLoc TL)
{
  TRACE;

  BuiltinType bt = translate_builtin_type (TL.getTypePtr ()->getKind ());

  return stack.push (mkBuiltinTypeLoc (bt));
}


bool
OCamlVisitor::TraverseAtomicTypeLoc (clang::AtomicTypeLoc TL)
{
  TRACE;

  ptr<Tloc> value = must_traverse (TL.getValueLoc ());

  return stack.push (mkAtomicTypeLoc (value));
}


bool
OCamlVisitor::TraverseTypeOfExprTypeLoc (clang::TypeOfExprTypeLoc TL)
{
  TRACE;

  ptr<Expr> expr = must_traverse (TL.getUnderlyingExpr ());

  return stack.push (mkTypeOfExprTypeLoc (expr));
}


bool
OCamlVisitor::TraverseTypeOfTypeLoc (clang::TypeOfTypeLoc TL)
{
  TRACE;

  ptr<Tloc> type = must_traverse (TL.getUnderlyingTInfo ()->getTypeLoc ());

  return stack.push (mkTypeOfTypeLoc (type));
}


bool
OCamlVisitor::TraverseConstantArrayTypeLoc (clang::ConstantArrayTypeLoc TL)
{
  TRACE;

  ptr<Tloc> element = must_traverse (TL.getElementLoc ());
  uint64_t size = TL.getTypePtr ()->getSize ().getZExtValue ();

  return stack.push (mkConstantArrayTypeLoc (element, size));
}


bool
OCamlVisitor::TraverseVariableArrayTypeLoc (clang::VariableArrayTypeLoc TL)
{
  TRACE;

  ptr<Tloc> element = must_traverse (TL.getElementLoc ());
  option<Expr> size = maybe_traverse (TL.getSizeExpr ());

  return stack.push (mkVariableArrayTypeLoc (element, size));
}


bool
OCamlVisitor::TraverseIncompleteArrayTypeLoc (clang::IncompleteArrayTypeLoc TL)
{
  TRACE;

  ptr<Tloc> element = must_traverse (TL.getElementLoc ());

  return stack.push (mkIncompleteArrayTypeLoc (element));
}


bool
OCamlVisitor::TraversePointerTypeLoc (clang::PointerTypeLoc TL)
{
  TRACE;

  ptr<Tloc> pointee = must_traverse (TL.getPointeeLoc ());

  return stack.push (mkPointerTypeLoc (pointee));
}


bool
OCamlVisitor::TraverseElaboratedTypeLoc (clang::ElaboratedTypeLoc TL)
{
  TRACE;

  TraverseNestedNameSpecifierLoc (TL.getQualifierLoc ());
  ptr<Tloc> type = must_traverse (TL.getNamedTypeLoc ());

  return stack.push (mkElaboratedTypeLoc (type));
}


bool
OCamlVisitor::TraverseQualifiedTypeLoc (clang::QualifiedTypeLoc TL)
{
  TRACE;

  ptr<Tloc> unqual = must_traverse (TL.getUnqualifiedLoc ());
  clang::Qualifiers quals = TL.getType ().getLocalQualifiers ();

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

  return stack.push (mkQualifiedTypeLoc (unqual, qualifiers, addressSpace));
}


bool
OCamlVisitor::TraverseEnumTypeLoc (clang::EnumTypeLoc TL)
{
  TRACE;

  clang::StringRef name = TL.getDecl ()->getName ();

  return stack.push (mkEnumTypeLoc (name));
}


bool
OCamlVisitor::TraverseRecordTypeLoc (clang::RecordTypeLoc TL)
{
  TRACE;

  TagTypeKind kind = translate_tag_type_kind (TL.getDecl ()->getTagKind ());
  clang::StringRef name = TL.getDecl ()->getName ();

  return stack.push (mkRecordTypeLoc (kind, name));
}


bool
OCamlVisitor::TraverseFunctionNoProtoTypeLoc (clang::FunctionNoProtoTypeLoc TL)
{
  TRACE;

  ptr<Tloc> result = must_traverse (TL.getResultLoc ());

  return stack.push (mkFunctionNoProtoTypeLoc (result));
}


bool
OCamlVisitor::TraverseFunctionProtoTypeLoc (clang::FunctionProtoTypeLoc TL)
{
  TRACE;

#if !CLANG_BUG_WAS_FIXED
  bool is_buggy = false;
  for (auto param : TL.getParams ())
    if (!param)
      is_buggy = true;

  if (is_buggy)
    {
      ptr<Tloc> result = must_traverse (TL.getResultLoc ());

      stack.push (mkFunctionNoProtoTypeLoc (result));
    }
  else
    {
#endif

      ptr<Tloc> result = must_traverse (TL.getResultLoc ());
      list<Decl> args = traverse_list (TL.getParams ());

      // TODO: exceptions

      stack.push (mkFunctionProtoTypeLoc (result, args));

#if !CLANG_BUG_WAS_FIXED
    }
#endif

  return true;
}


bool
OCamlVisitor::TraverseTypedefTypeLoc (clang::TypedefTypeLoc TL)
{
  TRACE;

  clang::StringRef name = TL.getTypedefNameDecl ()->getName ();

  return stack.push (mkTypedefTypeLoc (name));
}


bool
OCamlVisitor::TraverseParenTypeLoc (clang::ParenTypeLoc TL)
{
  TRACE;

  ptr<Tloc> inner = must_traverse (TL.getInnerLoc ());

  return stack.push (mkParenTypeLoc (inner));
}


#define UNIMP_TYPE_LOC(CLASS)						\
  bool									\
  OCamlVisitor::Traverse##CLASS##TypeLoc (clang::CLASS##TypeLoc TL)	\
  {									\
    TODO;                                                               \
    TRACE;								\
    Base::Traverse##CLASS##TypeLoc (TL);                                \
                                                                        \
    return stack.push (mk##CLASS##TypeLoc ());                          \
  }                                                                     \

UNIMP_TYPE_LOC (Auto)
UNIMP_TYPE_LOC (BlockPointer)


bool
OCamlVisitor::TraverseComplexTypeLoc (clang::ComplexTypeLoc TL)
{
  TRACE;

  ptr<Ctyp> element = must_traverse (TL.getTypePtr()->getElementType());

  return stack.push (mkComplexTypeLoc (element));
}


bool
OCamlVisitor::TraverseExtVectorTypeLoc (clang::ExtVectorTypeLoc TL)
{
  TRACE;

  ptr<Ctyp> elt_type = must_traverse (TL.getTypePtr ()->getElementType ());
  unsigned num_elts = TL.getTypePtr ()->getNumElements ();
  VectorKind vect_kind =
    translate_vector_kind (TL.getTypePtr ()->getVectorKind ());

  return stack.push (mkExtVectorTypeLoc (elt_type, num_elts, vect_kind));
}


bool
OCamlVisitor::TraverseVectorTypeLoc (clang::VectorTypeLoc TL)
{
  TRACE;

  ptr<Ctyp> elt_type = must_traverse (TL.getTypePtr ()->getElementType ());
  unsigned num_elts = TL.getTypePtr ()->getNumElements ();
  VectorKind vect_kind =
    translate_vector_kind (TL.getTypePtr ()->getVectorKind ());

  return stack.push (mkVectorTypeLoc (elt_type, num_elts, vect_kind));
}


bool
OCamlVisitor::TraverseAttributedTypeLoc (clang::AttributedTypeLoc TL)
{
  TRACE;

  AttributedTypeKind attr_kind =
    translate_attributed_type_kind (TL.getAttrKind ());
  ptr<Tloc> modified_loc = must_traverse (TL.getModifiedLoc ());
  option<Expr> expr;
  if (TL.hasAttrExprOperand ())
    expr = must_traverse (TL.getAttrExprOperand ());

  return stack.push (mkAttributedTypeLoc (attr_kind, modified_loc, expr));
}


bool
OCamlVisitor::TraverseDecayedTypeLoc (clang::DecayedTypeLoc TL)
{
  TRACE;

  ptr<Tloc> original = must_traverse (TL.getOriginalLoc ());

  return stack.push (mkDecayedTypeLoc (original));
}


bool
OCamlVisitor::TraverseDecltypeTypeLoc (clang::DecltypeTypeLoc TL)
{
  TRACE;

  ptr<Expr> expr = must_traverse (TL.getUnderlyingExpr ());

  return stack.push (mkDecltypeTypeLoc (expr));
}

bool
OCamlVisitor::TraverseObjCObjectPointerTypeLoc
(clang::ObjCObjectPointerTypeLoc TL)
{
  TRACE;

  ptr<Tloc> pointee = must_traverse (TL.getPointeeLoc ());

  return stack.push (mkObjCObjectPointerTypeLoc (pointee));
}

bool
OCamlVisitor::TraverseObjCObjectTypeLoc (clang::ObjCObjectTypeLoc TL)
{
  TRACE;

  ptr<Tloc> base = must_traverse (TL.getBaseLoc ());

  return stack.push (mkObjCObjectTypeLoc (base));
}

bool
OCamlVisitor::TraverseObjCInterfaceTypeLoc (clang::ObjCInterfaceTypeLoc TL)
{
  TRACE;

  clang::StringRef name = TL.getIFaceDecl ()->getName ();

  return stack.push (mkObjCInterfaceTypeLoc (name));
}


UNIMP_TYPE_LOC (DependentName)
UNIMP_TYPE_LOC (DependentSizedArray)
UNIMP_TYPE_LOC (DependentSizedExtVector)
UNIMP_TYPE_LOC (DependentTemplateSpecialization)
UNIMP_TYPE_LOC (InjectedClassName)
UNIMP_TYPE_LOC (LValueReference)
UNIMP_TYPE_LOC (MemberPointer)
UNIMP_TYPE_LOC (PackExpansion)
UNIMP_TYPE_LOC (RValueReference)
UNIMP_TYPE_LOC (SubstTemplateTypeParm)
UNIMP_TYPE_LOC (SubstTemplateTypeParmPack)
UNIMP_TYPE_LOC (TemplateSpecialization)
bool
OCamlVisitor::TraverseTemplateTypeParmTypeLoc
(clang::TemplateTypeParmTypeLoc TL)
{
  TRACE;

  clang::StringRef name = TL.getDecl ()->getName ();

  return stack.push (mkTemplateTypeParmTypeLoc (name));
}


UNIMP_TYPE_LOC (UnaryTransform)
UNIMP_TYPE_LOC (UnresolvedUsing)

// }}}
