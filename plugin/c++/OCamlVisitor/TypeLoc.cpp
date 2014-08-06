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
      printf ("WARNING: %s creates dummy TypeLoc, as derived function did not produce any\n",
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
  stack.push (type_loc);

  return true;
}


bool
OCamlVisitor::TraverseBuiltinTypeLoc (clang::BuiltinTypeLoc TL)
{
  TRACE;

  BuiltinType bt = translate_builtin_type (TL.getTypePtr ()->getKind ());

  stack.push (mkBuiltinTypeLoc (bt));

  return true;
}


bool
OCamlVisitor::TraverseAtomicTypeLoc (clang::AtomicTypeLoc TL)
{
  TRACE;

  ptr<Tloc> value = must_traverse (TL.getValueLoc ());

  stack.push (mkAtomicTypeLoc (value));

  return true;
}


bool
OCamlVisitor::TraverseTypeOfExprTypeLoc (clang::TypeOfExprTypeLoc TL)
{
  TRACE;

  ptr<Expr> expr = must_traverse (TL.getUnderlyingExpr ());

  stack.push (mkTypeOfExprTypeLoc (expr));

  return true;
}


bool
OCamlVisitor::TraverseTypeOfTypeLoc (clang::TypeOfTypeLoc TL)
{
  TRACE;

  ptr<Tloc> type = must_traverse (TL.getUnderlyingTInfo ()->getTypeLoc ());

  stack.push (mkTypeOfTypeLoc (type));

  return true;
}


bool
OCamlVisitor::TraverseConstantArrayTypeLoc (clang::ConstantArrayTypeLoc TL)
{
  TRACE;

  ptr<Tloc> element = must_traverse (TL.getElementLoc ());
  uint64_t size = TL.getTypePtr ()->getSize ().getZExtValue ();

  stack.push (mkConstantArrayTypeLoc (element, size));

  return true;
}


bool
OCamlVisitor::TraverseVariableArrayTypeLoc (clang::VariableArrayTypeLoc TL)
{
  TRACE;

  ptr<Tloc> element = must_traverse (TL.getElementLoc ());
  option<Expr> size = maybe_traverse (TL.getSizeExpr ());

  stack.push (mkVariableArrayTypeLoc (element, size));

  return true;
}


bool
OCamlVisitor::TraverseIncompleteArrayTypeLoc (clang::IncompleteArrayTypeLoc TL)
{
  TRACE;

  ptr<Tloc> element = must_traverse (TL.getElementLoc ());

  stack.push (mkIncompleteArrayTypeLoc (element));

  return true;
}


bool
OCamlVisitor::TraversePointerTypeLoc (clang::PointerTypeLoc TL)
{
  TRACE;

  ptr<Tloc> pointee = must_traverse (TL.getPointeeLoc ());

  stack.push (mkPointerTypeLoc (pointee));

  return true;
}


bool
OCamlVisitor::TraverseElaboratedTypeLoc (clang::ElaboratedTypeLoc TL)
{
  TRACE;

  TraverseNestedNameSpecifierLoc (TL.getQualifierLoc ());
  ptr<Tloc> type = must_traverse (TL.getNamedTypeLoc ());

  stack.push (mkElaboratedTypeLoc (type));

  return true;
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

  stack.push (mkQualifiedTypeLoc (unqual, qualifiers, addressSpace));

  return true;
}


bool
OCamlVisitor::TraverseEnumTypeLoc (clang::EnumTypeLoc TL)
{
  TRACE;

  clang::StringRef name = TL.getDecl ()->getName ();

  stack.push (mkEnumTypeLoc (name));

  return true;
}


bool
OCamlVisitor::TraverseRecordTypeLoc (clang::RecordTypeLoc TL)
{
  TRACE;

  TagTypeKind kind = translate_tag_type_kind (TL.getDecl ()->getTagKind ());
  clang::StringRef name = TL.getDecl ()->getName ();

  stack.push (mkRecordTypeLoc (kind, name));

  return true;
}


bool
OCamlVisitor::TraverseFunctionNoProtoTypeLoc (clang::FunctionNoProtoTypeLoc TL)
{
  TRACE;

  ptr<Tloc> result = must_traverse (TL.getResultLoc ());

  stack.push (mkFunctionNoProtoTypeLoc (result));

  return true;
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

  stack.push (mkTypedefTypeLoc (name));

  return true;
}


bool
OCamlVisitor::TraverseParenTypeLoc (clang::ParenTypeLoc TL)
{
  TRACE;

  ptr<Tloc> inner = must_traverse (TL.getInnerLoc ());

  stack.push (mkParenTypeLoc (inner));

  return true;
}


#define UNIMP_TYPE_LOC(CLASS)						\
bool									\
OCamlVisitor::Traverse##CLASS##TypeLoc (clang::CLASS##TypeLoc TL)	\
{									\
  TODO;									\
  TRACE;								\
  Base::Traverse##CLASS##TypeLoc (TL);					\
  stack.push (mk##CLASS##TypeLoc ());					\
  return true;								\
}

UNIMP_TYPE_LOC (Auto)
UNIMP_TYPE_LOC (BlockPointer)


bool
OCamlVisitor::TraverseComplexTypeLoc (clang::ComplexTypeLoc TL)
{
  TRACE;

  ptr<Ctyp> element = must_traverse (TL.getTypePtr()->getElementType());

  stack.push (mkComplexTypeLoc (element));

  return true;
}


bool
OCamlVisitor::TraverseExtVectorTypeLoc (clang::ExtVectorTypeLoc TL)
{
  TRACE;

  ptr<Ctyp> elt_type = must_traverse (TL.getTypePtr ()->getElementType ());
  unsigned num_elts = TL.getTypePtr ()->getNumElements ();
  VectorKind vect_kind =
    translate_vector_kind (TL.getTypePtr ()->getVectorKind ());

  stack.push (mkExtVectorTypeLoc (elt_type, num_elts, vect_kind));

  return true;
}


bool
OCamlVisitor::TraverseVectorTypeLoc (clang::VectorTypeLoc TL)
{
  TRACE;

  ptr<Ctyp> elt_type = must_traverse (TL.getTypePtr ()->getElementType ());
  unsigned num_elts = TL.getTypePtr ()->getNumElements ();
  VectorKind vect_kind =
    translate_vector_kind (TL.getTypePtr ()->getVectorKind ());

  stack.push (mkVectorTypeLoc (elt_type, num_elts, vect_kind));

  return true;
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

  stack.push (mkAttributedTypeLoc (attr_kind, modified_loc, expr));

  return true;
}


bool
OCamlVisitor::TraverseDecayedTypeLoc (clang::DecayedTypeLoc TL)
{
  TRACE;

  ptr<Tloc> original = must_traverse (TL.getOriginalLoc ());

  stack.push (mkDecayedTypeLoc (original));

  return true;
}


bool
OCamlVisitor::TraverseDecltypeTypeLoc (clang::DecltypeTypeLoc TL)
{
  TRACE;

  ptr<Expr> expr = must_traverse (TL.getUnderlyingExpr ());

  stack.push (mkDecltypeTypeLoc (expr));

  return true;
}

bool
OCamlVisitor::TraverseObjCObjectPointerTypeLoc (clang::ObjCObjectPointerTypeLoc TL)
{
  TRACE;

  ptr<Tloc> pointee = must_traverse (TL.getPointeeLoc ());

  stack.push (mkObjCObjectPointerTypeLoc (pointee));

  return true;
}

bool
OCamlVisitor::TraverseObjCObjectTypeLoc (clang::ObjCObjectTypeLoc TL)
{
  TRACE;

  ptr<Tloc> base = must_traverse (TL.getBaseLoc ());

  stack.push (mkObjCObjectTypeLoc (base));

  return true;
}

bool
OCamlVisitor::TraverseObjCInterfaceTypeLoc (clang::ObjCInterfaceTypeLoc TL)
{
  TRACE;

  clang::StringRef name = TL.getIFaceDecl ()->getName ();

  stack.push (mkObjCInterfaceTypeLoc (name));

  return true;
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
OCamlVisitor::TraverseTemplateTypeParmTypeLoc (clang::TemplateTypeParmTypeLoc TL)
{
  TRACE;

  clang::StringRef name = TL.getDecl ()->getName ();

  stack.push (mkTemplateTypeParmTypeLoc (name));

  return true;
}


UNIMP_TYPE_LOC (UnaryTransform)
UNIMP_TYPE_LOC (UnresolvedUsing)

// }}}
