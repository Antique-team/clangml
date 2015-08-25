#include "OCamlVisitor.h"


/****************************************************
 * {{{1 Unary/binary operators
 */

bool
OCamlVisitor::TraverseUnaryOperator (clang::UnaryOperator *S)
{
  TRACE;

  UnaryOperator op = translate_unary_operator_kind (S->getOpcode ());
  ptr<Expr> subExpr = must_traverse (S->getSubExpr ());

  return stack.push (mkUnaryOperator (op, subExpr));
}


bool
OCamlVisitor::TraverseBinaryOperator (clang::BinaryOperator *S)
{
  TRACE;

  BinaryOperator op = translate_binary_operator_kind (S->getOpcode ());
  ptr<Expr> lhs = must_traverse (S->getLHS ());
  ptr<Expr> rhs = must_traverse (S->getRHS ());

  return stack.push (mkBinaryOperator (op, lhs, rhs));
}


bool
OCamlVisitor::TraverseCompoundAssignOperator (clang::CompoundAssignOperator *S)
{
  return TraverseBinaryOperator (S);
}


bool
OCamlVisitor::TraverseConditionalOperator (clang::ConditionalOperator *S)
{
  TRACE;

  ptr<Expr> cond = must_traverse (S->getCond ());
  ptr<Expr> trueExpr = must_traverse (S->getTrueExpr ());
  ptr<Expr> falseExpr = must_traverse (S->getFalseExpr ());

  return stack.push (mkConditionalOperator (cond, trueExpr, falseExpr));
}


bool
OCamlVisitor::TraverseBinaryConditionalOperator
(clang::BinaryConditionalOperator *S)
{
  TRACE;

  ptr<Expr> cond = must_traverse (S->getCond ());
  ptr<Expr> falseExpr = must_traverse (S->getFalseExpr ());

  return stack.push (mkBinaryConditionalOperator (cond, falseExpr));
}


bool
OCamlVisitor::TraverseOpaqueValueExpr (clang::OpaqueValueExpr *S)
{
  TRACE;

  ptr<Expr> sourceExpr = must_traverse (S->getSourceExpr ());

  return stack.push (mkOpaqueValueExpr (sourceExpr));
}


// }}}


/****************************************************
 * {{{1 Literals
 */

bool
OCamlVisitor::TraverseIntegerLiteral (clang::IntegerLiteral *S)
{
  return stack.push (mkIntegerLiteral
                     (S->getValue ().getSExtValue ()));
}


bool
OCamlVisitor::TraverseCharacterLiteral (clang::CharacterLiteral *S)
{
  return stack.push (mkCharacterLiteral
                     (S->getValue ()));
}


bool
OCamlVisitor::TraverseFloatingLiteral (clang::FloatingLiteral *S)
{
  // TODO: using approximate value here; should be using exact format
  return stack.push (mkFloatingLiteral
                     (S->getValueAsApproximateDouble ()));
}


bool
OCamlVisitor::TraverseStringLiteral (clang::StringLiteral *S)
{
  return stack.push (mkStringLiteral
                     (S->getBytes ()));
}


bool
OCamlVisitor::TraverseObjCStringLiteral (clang::ObjCStringLiteral *S)
{
  return TraverseStringLiteral (S->getString ());
}


bool
OCamlVisitor::TraverseObjCBoolLiteralExpr (clang::ObjCBoolLiteralExpr *S)
{
  return stack.push (mkObjCBoolLiteralExpr (S->getValue ()));
}


bool
OCamlVisitor::TraverseCXXBoolLiteralExpr (clang::CXXBoolLiteralExpr *S)
{
  return stack.push (mkCXXBoolLiteralExpr (S->getValue ()));
}


bool
OCamlVisitor::TraverseImaginaryLiteral (clang::ImaginaryLiteral *S)
{
  TRACE;

  ptr<Expr> sub = must_traverse (S->getSubExpr ());

  return stack.push (mkImaginaryLiteral (sub));
}


// }}}

/****************************************************
 * {{{1 Expressions
 */


bool
OCamlVisitor::TraverseImplicitValueInitExpr (clang::ImplicitValueInitExpr *S)
{
  TRACE;

  return stack.push (mkImplicitValueInitExpr ());
}


bool
OCamlVisitor::TraverseArraySubscriptExpr (clang::ArraySubscriptExpr *S)
{
  TRACE;

  ptr<Expr> base = must_traverse (S->getBase ());
  ptr<Expr> idx = must_traverse (S->getIdx ());

  return stack.push (mkArraySubscriptExpr (base, idx));
}


bool
OCamlVisitor::TraverseStmtExpr (clang::StmtExpr *S)
{
  TRACE;

  ptr<Stmt> stmt = must_traverse (S->getSubStmt ());

  return stack.push (mkStmtExpr (stmt));
}


bool
OCamlVisitor::TraverseDeclRefExpr (clang::DeclRefExpr *S)
{
  TRACE;

  clang::StringRef name = getName (S);

  return stack.push (mkDeclRefExpr (name));
}


bool
OCamlVisitor::TraversePredefinedExpr (clang::PredefinedExpr *S)
{
  TRACE;

  PredefinedExpr kind = translate_predefined_expr (S->getIdentType ());

  return stack.push (mkPredefinedExpr (kind));
}


bool
OCamlVisitor::TraverseCStyleCastExpr (clang::CStyleCastExpr *S)
{
  TRACE;

  CastKind kind = translate_cast_kind (S->getCastKind ());
  ptr<Tloc> type = must_traverse (S->getTypeInfoAsWritten ()->getTypeLoc ());
  ptr<Expr> sub = must_traverse (S->getSubExpr ());

  return stack.push (mkCStyleCastExpr (kind, type, sub));
}


bool
OCamlVisitor::TraverseImplicitCastExpr (clang::ImplicitCastExpr *S)
{
  TRACE;

  CastKind kind = translate_cast_kind (S->getCastKind ());
  ptr<Expr> sub = must_traverse (S->getSubExpr ());

  return stack.push (mkImplicitCastExpr (kind, sub));
}


bool
OCamlVisitor::TraverseParenExpr (clang::ParenExpr *S)
{
  TRACE;

  ptr<Expr> sub = must_traverse (S->getSubExpr ());

  return stack.push (mkParenExpr (sub));
}


bool
OCamlVisitor::TraverseCompoundLiteralExpr (clang::CompoundLiteralExpr *S)
{
  TRACE;

  ptr<Tloc> type = getTypeLoc (S);
  ptr<Expr> init = must_traverse (S->getInitializer ());

  return stack.push (mkCompoundLiteralExpr (type, init));
}


bool
OCamlVisitor::TraverseDesignator (clang::DesignatedInitExpr::Designator D,
                                  clang::DesignatedInitExpr *S)
{
  TRACE;

  ptr<Desg_> dr;

  if (D.isFieldDesignator ())
    {
      clang::FieldDecl *field = D.getField ();
      assert (field);
      clang::StringRef name = field->getName ();

      dr = mkFieldDesignator (name);
    }
  else if (D.isArrayDesignator ())
    {
      ptr<Expr> index = must_traverse (S->getArrayIndex (D));

      dr = mkArrayDesignator (index);
    }
  else if (D.isArrayRangeDesignator ())
    {
      ptr<Expr> start = must_traverse (S->getArrayRangeStart (D));
      ptr<Expr> end   = must_traverse (S->getArrayRangeEnd (D));

      dr = mkArrayRangeDesignator (start, end);
    }
  else
    assert (!"Invalid or unknown designator");

  ptr<Desg> designator = mkDesg ();
  designator->dr = dr;
  designator->dr_sloc = sloc (S);

  return stack.push (designator);
}


bool
OCamlVisitor::TraverseDesignatedInitExpr (clang::DesignatedInitExpr *S)
{
  TRACE;

  list<Desg> designators = traverse_list (designator_range (S), S);
  ptr<Expr> init = must_traverse (S->getInit ());

  return stack.push (mkDesignatedInitExpr (designators, init));
}


bool
OCamlVisitor::TraverseInitListExpr (clang::InitListExpr *S)
{
  TRACE;

  list<Expr> inits = traverse_list (S->children ());

  return stack.push (mkInitListExpr (inits));
}


bool
OCamlVisitor::TraverseVAArgExpr (clang::VAArgExpr *S)
{
  TRACE;

  ptr<Expr> sub = must_traverse (S->getSubExpr ());
  ptr<Tloc> type = must_traverse (S->getWrittenTypeInfo ()->getTypeLoc ());

  return stack.push (mkVAArgExpr (sub, type));
}


bool
OCamlVisitor::TraverseCallExpr (clang::CallExpr *S)
{
  TRACE;

  ptr<Expr> callee = must_traverse (S->getCallee ());
  list<Expr> args = traverse_list (arg_range (S));

  return stack.push (mkCallExpr (callee, args));
}


bool
OCamlVisitor::TraverseMemberExpr (clang::MemberExpr *S)
{
  TRACE;

  ptr<Expr> base = must_traverse (S->getBase ());
  clang::StringRef member = S->getMemberDecl ()->getName ();
  bool isArrow = S->isArrow ();

  return stack.push (mkMemberExpr (base, member, isArrow));
}


template<typename TypeFactory, typename ExprFactory>
void
OCamlVisitor::pushUnaryExprOrTypeTraitExpr (clang::UnaryExprOrTypeTraitExpr *S,
                                            TypeFactory mkOfType,
                                            ExprFactory mkOfExpr)
{
  ptr<Expr_> expr;
  if (S->isArgumentType ())
    expr = mkOfType (must_traverse (S->getArgumentTypeInfo ()->getTypeLoc ()));
  else
    expr = mkOfExpr (must_traverse (S->getArgumentExpr ()));
  stack.push (expr);
}


bool
OCamlVisitor::TraverseUnaryExprOrTypeTraitExpr
(clang::UnaryExprOrTypeTraitExpr *S)
{
  TRACE;

  switch (S->getKind ())
    {
    case clang::UETT_SizeOf:
      pushUnaryExprOrTypeTraitExpr (S, mkSizeOfType, mkSizeOfExpr);
      break;
    case clang::UETT_AlignOf:
      pushUnaryExprOrTypeTraitExpr (S, mkAlignOfType, mkAlignOfExpr);
      break;
    case clang::UETT_VecStep:
      pushUnaryExprOrTypeTraitExpr (S, mkVecStepType, mkVecStepExpr);
      break;
    }

  return true;
}


bool
OCamlVisitor::TraverseAddrLabelExpr (clang::AddrLabelExpr *S)
{
  TRACE;

  clang::StringRef label = S->getLabel ()->getName ();

  return stack.push (mkAddrLabelExpr (label));
}


bool
OCamlVisitor::TraverseOffsetOfNode (clang::OffsetOfExpr::OffsetOfNode N,
                                    clang::OffsetOfExpr *S)
{
  TRACE;

  ptr<OffsetofNode> node;

  switch (N.getKind ())
    {
    case clang::OffsetOfExpr::OffsetOfNode::Array:
      node = mkOON_Array
        (must_traverse (S->getIndexExpr (N.getArrayExprIndex ())));
      break;
    case clang::OffsetOfExpr::OffsetOfNode::Field:
      node = mkOON_Field (N.getField ()->getName ());
      break;
    case clang::OffsetOfExpr::OffsetOfNode::Identifier:
      node = mkOON_Identifier (N.getFieldName ()->getName ());
      break;
    case clang::OffsetOfExpr::OffsetOfNode::Base:
      node = mkOON_Base (must_traverse (*N.getBase ()));
      break;
    }

  return stack.push (node);
}


bool
OCamlVisitor::TraverseOffsetOfExpr (clang::OffsetOfExpr *S)
{
  TRACE;

  ptr<Tloc> type = must_traverse (S->getTypeSourceInfo ()->getTypeLoc ());
  list<OffsetofNode> components = traverse_list (offsetof_node_range (S), S);

  return stack.push (mkOffsetOfExpr (type, components));
}


bool
OCamlVisitor::TraverseExtVectorElementExpr (clang::ExtVectorElementExpr *S)
{
  TRACE;

  ptr<Expr> base = must_traverse (S->getBase ());
  clang::StringRef accessor = S->getAccessor ().getName ();

  return stack.push (mkExtVectorElementExpr (base, accessor));
}


bool
OCamlVisitor::TraverseAtomicExpr (clang::AtomicExpr *S)
{
  TRACE;

  AtomicOp op = translate_atomic_op (S->getOp ());
  list<Expr> subExprs = traverse_list (S->children ());

  return stack.push (mkAtomicExpr (op, subExprs));
}


bool
OCamlVisitor::TraverseShuffleVectorExpr (clang::ShuffleVectorExpr *S)
{
  TRACE;

  list<Expr> subExprs = traverse_list (S->children ());

  return stack.push (mkShuffleVectorExpr (subExprs));
}


// bool
// OCamlVisitor::TraverseBinaryTypeTraitExpr (clang::BinaryTypeTraitExpr *S)
// {
//   TRACE;

//   BinaryTypeTrait trait = translate_binary_type_trait (S->getTrait ());
//   ptr<Ctyp> lhs = must_traverse (S->getLhsType ());
//   ptr<Ctyp> rhs = must_traverse (S->getRhsType ());

//   return stack.push (mkBinaryTypeTraitExpr (trait, lhs, rhs));
// }


// bool
// OCamlVisitor::TraverseUnaryTypeTraitExpr (clang::UnaryTypeTraitExpr *S)
// {
//   TRACE;

//   UnaryTypeTrait trait = translate_unary_type_trait (S->getTrait ());
//   ptr<Ctyp> queried = must_traverse (S->getQueriedType ());

//   return stack.push (mkUnaryTypeTraitExpr (trait, queried));
// }


bool
OCamlVisitor::TraverseArrayTypeTraitExpr (clang::ArrayTypeTraitExpr *S)
{
  TRACE;

  ArrayTypeTrait trait = translate_array_type_trait (S->getTrait ());
  ptr<Ctyp> queried = must_traverse (S->getQueriedType ());
  option<Expr> dimension = maybe_traverse (S->getDimensionExpression ());

  return stack.push (mkArrayTypeTraitExpr (trait, queried, dimension));
}


bool
OCamlVisitor::TraverseConvertVectorExpr (clang::ConvertVectorExpr *S)
{
  TRACE;

  ptr<Expr> src  = must_traverse (S->getSrcExpr ());
  ptr<Ctyp> type = must_traverse (S->getTypeSourceInfo ()->getType());

  return stack.push (mkConvertVectorExpr (src, type));
}


bool
OCamlVisitor::TraverseChooseExpr (clang::ChooseExpr *S)
{
  TRACE;

  ptr<Expr> cond = must_traverse (S->getCond ());
  ptr<Expr> lhs  = must_traverse (S->getLHS ());
  ptr<Expr> rhs  = must_traverse (S->getRHS ());

  return stack.push (mkChooseExpr (cond, lhs, rhs));
}


UNIMP_STMT (Expr, AsTypeExpr)
UNIMP_STMT (Expr, BlockExpr)
UNIMP_STMT (Expr, CUDAKernelCallExpr)
UNIMP_STMT (Expr, CXXBindTemporaryExpr)
UNIMP_STMT (Expr, CXXConstCastExpr)
UNIMP_STMT (Expr, CXXConstructExpr)
UNIMP_STMT (Expr, CXXDefaultArgExpr)
UNIMP_STMT (Expr, CXXDefaultInitExpr)
UNIMP_STMT (Expr, CXXDeleteExpr)
UNIMP_STMT (Expr, CXXDependentScopeMemberExpr)
UNIMP_STMT (Expr, CXXDynamicCastExpr)
UNIMP_STMT (Expr, CXXFunctionalCastExpr)
UNIMP_STMT (Expr, CXXMemberCallExpr)
UNIMP_STMT (Expr, CXXNewExpr)
UNIMP_STMT (Expr, CXXNoexceptExpr)


bool
OCamlVisitor::TraverseCXXNullPtrLiteralExpr (clang::CXXNullPtrLiteralExpr *S)
{
  TRACE;

  return stack.push (mkCXXNullPtrLiteralExpr ());
}


bool
OCamlVisitor::TraverseGNUNullExpr (clang::GNUNullExpr *S)
{
  TRACE;

  ptr<Ctyp> type = must_traverse (S->getType ());

  return stack.push (mkGNUNullExpr (type));
}


bool
OCamlVisitor::TraverseObjCMessageExpr (clang::ObjCMessageExpr *S)
{
  TRACE;

  option<Ctyp> class_receiver = maybe_traverse (S->getClassReceiver ());

  option<Expr> instance_receiver = maybe_traverse (S->getInstanceReceiver ());

  clang::StringRef selector = S->getSelector ().getNameForSlot (0);

  list<Expr> args;
  for (unsigned i = 0; i < S->getNumArgs (); ++i)
    args.push_back (must_traverse (S->getArg (i)));

  return stack.push (mkObjCMessageExpr
                     (instance_receiver, class_receiver, selector, args));
}


bool
OCamlVisitor::TraverseObjCArrayLiteral (clang::ObjCArrayLiteral *S)
{
  TRACE;

  list<Expr> elements;
  for (unsigned i = 0; i < S->getNumElements (); ++i)
    elements.push_back (must_traverse (S->getElement (i)));

  return stack.push (mkObjCArrayLiteral (elements));
}


bool
OCamlVisitor::TraverseObjCDictionaryLiteral (clang::ObjCDictionaryLiteral *S)
{
  TRACE;

  std::vector<std::tuple<ptr<Expr>, ptr<Expr>>> map;
  
  for (unsigned i = 0; i < S->getNumElements (); ++i)
    {
      ptr<Expr> key   = must_traverse (S->getKeyValueElement (i).Key  );
      ptr<Expr> value = must_traverse (S->getKeyValueElement (i).Value);
      map.emplace_back (key, value);
    }

  return stack.push (mkObjCDictionaryLiteral (map));
}


bool
OCamlVisitor::TraverseObjCEncodeExpr (clang::ObjCEncodeExpr *S)
{
  TRACE;

  ptr<Ctyp> encoded_type = must_traverse (S->getEncodedType ());

  return stack.push (mkObjCEncodeExpr (encoded_type));
}


bool
OCamlVisitor::TraverseObjCIvarRefExpr (clang::ObjCIvarRefExpr *S)
{
  TRACE;

  ptr<Expr> base = must_traverse (S->getBase ());

  ptr<Decl> decl = must_traverse (S->getDecl ());

  bool isArrow = S->isArrow ();

  bool isFreeIvar = S->isFreeIvar ();

  return stack.push (mkObjCIvarRefExpr (base, decl, isArrow, isFreeIvar));
}


bool
OCamlVisitor::TraverseObjCBoxedExpr (clang::ObjCBoxedExpr *S)
{
  TRACE;

  ptr<Expr> sub_expr = must_traverse (S->getSubExpr ());

  return stack.push (mkObjCBoxedExpr (sub_expr));
}


bool
OCamlVisitor::TraverseObjCPropertyRefExpr (clang::ObjCPropertyRefExpr *S)
{
  TRACE;

  ptr<Expr> base = must_traverse (S->getBase ());

  return stack.push (mkObjCPropertyRefExpr (base));
}


bool
OCamlVisitor::TraverseObjCIsaExpr (clang::ObjCIsaExpr *S)
{
  TRACE;

  ptr<Expr> base = must_traverse (S->getBase ());

  return stack.push (mkObjCIsaExpr (base));
}


bool
OCamlVisitor::TraversePseudoObjectExpr (clang::PseudoObjectExpr *S)
{
  TRACE;

  ptr<Expr> syntactic = must_traverse (S->getSyntacticForm ());
  list<Expr> semantic;
  option<Expr> result = maybe_traverse (S->getResultExpr ());

  for (unsigned i = 0; i < S->getNumSemanticExprs (); ++i) {
    semantic.push_back (must_traverse (S->getSemanticExpr (i)));
  }

  return stack.push (mkPseudoObjectExpr (syntactic, semantic, result));
}


bool
OCamlVisitor::TraverseObjCSelectorExpr (clang::ObjCSelectorExpr *S)
{
  TRACE;

  clang::StringRef selector = S->getSelector ().getNameForSlot (0);

  return stack.push (mkObjCSelectorExpr (selector));
}


bool
OCamlVisitor::TraverseObjCProtocolExpr (clang::ObjCProtocolExpr *S)
{
  TRACE;

  clang::StringRef protocol = S->getProtocol ()->getName ();

  return stack.push (mkObjCProtocolExpr (protocol));
}


bool
OCamlVisitor::TraverseGenericSelectionExpr (clang::GenericSelectionExpr *S)
{
  TRACE;

  ptr<Expr> controlling = must_traverse (S->getControllingExpr ());
  std::vector<std::tuple<ptr<Expr>, option<Ctyp>>> assoc_list;
  ptr<Expr> result = maybe_traverse (S->getResultExpr ());

  for (unsigned i = 0; i < S->getNumAssocs (); ++i) {
    ptr<Expr> key = must_traverse (S->getAssocExpr (i));
    // the default case has no associated type in
    // _Generic(1.0, double: 1, float: 2, default: 3);
    // hence we use an option
    option<Ctyp> value = maybe_traverse (S->getAssocType (i));
    assoc_list.emplace_back (key, value);
  }

  return stack.push (mkGenericSelectionExpr (controlling, assoc_list, result));
}


UNIMP_STMT (Expr, CXXOperatorCallExpr)
UNIMP_STMT (Expr, CXXPseudoDestructorExpr)
UNIMP_STMT (Expr, CXXReinterpretCastExpr)
UNIMP_STMT (Expr, CXXScalarValueInitExpr)
UNIMP_STMT (Expr, CXXStaticCastExpr)
UNIMP_STMT (Expr, CXXStdInitializerListExpr)
UNIMP_STMT (Expr, CXXTemporaryObjectExpr)
UNIMP_STMT (Expr, CXXThisExpr)
UNIMP_STMT (Expr, CXXThrowExpr)
UNIMP_STMT (Expr, CXXTypeidExpr)
UNIMP_STMT (Expr, CXXUnresolvedConstructExpr)
UNIMP_STMT (Expr, CXXUuidofExpr)
UNIMP_STMT (Expr, DependentScopeDeclRefExpr)
UNIMP_STMT (Expr, ExpressionTraitExpr)
UNIMP_STMT (Expr, ExprWithCleanups)
UNIMP_STMT (Expr, FunctionParmPackExpr)
UNIMP_STMT (Expr, LambdaExpr)
UNIMP_STMT (Expr, MaterializeTemporaryExpr)
UNIMP_STMT (Expr, MSPropertyRefExpr)
UNIMP_STMT (Expr, ObjCBridgedCastExpr)
UNIMP_STMT (Expr, ObjCIndirectCopyRestoreExpr)
UNIMP_STMT (Expr, ObjCSubscriptRefExpr)
UNIMP_STMT (Expr, PackExpansionExpr)
UNIMP_STMT (Expr, ParenListExpr)
UNIMP_STMT (Expr, SizeOfPackExpr)
UNIMP_STMT (Expr, SubstNonTypeTemplateParmExpr)
UNIMP_STMT (Expr, SubstNonTypeTemplateParmPackExpr)
UNIMP_STMT (Expr, TypeTraitExpr)
UNIMP_STMT (Expr, UnresolvedLookupExpr)
UNIMP_STMT (Expr, UnresolvedMemberExpr)
UNIMP_STMT (Expr, UserDefinedLiteral)


// }}}
