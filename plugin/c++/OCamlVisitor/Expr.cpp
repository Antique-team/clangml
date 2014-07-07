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

  stack.push (mkUnaryOperator (op, subExpr));

  return true;
}


bool
OCamlVisitor::TraverseBinaryOperator (clang::BinaryOperator *S)
{
  TRACE;

  BinaryOperator op = translate_binary_operator_kind (S->getOpcode ());
  ptr<Expr> lhs = must_traverse (S->getLHS ());
  ptr<Expr> rhs = must_traverse (S->getRHS ());

  stack.push (mkBinaryOperator (op, lhs, rhs));

  return true;
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

  stack.push (mkConditionalOperator (cond, trueExpr, falseExpr));

  return true;
}

bool
OCamlVisitor::TraverseBinaryConditionalOperator (clang::BinaryConditionalOperator *S)
{
  TRACE;

  ptr<Expr> cond = must_traverse (S->getCond ());
  ptr<Expr> falseExpr = must_traverse (S->getFalseExpr ());

  stack.push (mkBinaryConditionalOperator (cond, falseExpr));

  return true;
}

bool
OCamlVisitor::TraverseOpaqueValueExpr (clang::OpaqueValueExpr *S)
{
  TRACE;

  ptr<Expr> sourceExpr = must_traverse (S->getSourceExpr ());

  stack.push (mkOpaqueValueExpr (sourceExpr));

  return true;
}

// }}}


/****************************************************
 * {{{1 Literals
 */

bool
OCamlVisitor::TraverseIntegerLiteral (clang::IntegerLiteral *S)
{
  stack.push (mkIntegerLiteral
              (S->getValue ().getSExtValue ()));

  return true;
}


bool
OCamlVisitor::TraverseCharacterLiteral (clang::CharacterLiteral *S)
{
  stack.push (mkCharacterLiteral
              (S->getValue ()));

  return true;
}


bool
OCamlVisitor::TraverseFloatingLiteral (clang::FloatingLiteral *S)
{
  // TODO: using approximate value here; should be using exact format
  stack.push (mkFloatingLiteral
              (S->getValueAsApproximateDouble ()));

  return true;
}


bool
OCamlVisitor::TraverseStringLiteral (clang::StringLiteral *S)
{
  stack.push (mkStringLiteral
              (S->getBytes ()));

  return true;
}


bool
OCamlVisitor::TraverseImaginaryLiteral (clang::ImaginaryLiteral *S)
{
  TRACE;

  ptr<Expr> sub = must_traverse (S->getSubExpr ());

  stack.push (mkImaginaryLiteral (sub));

  return true;
}


// }}}

/****************************************************
 * {{{1 Expressions
 */


bool
OCamlVisitor::TraverseImplicitValueInitExpr (clang::ImplicitValueInitExpr *S)
{
  TRACE;

  stack.push (mkImplicitValueInitExpr ());

  return true;
}


bool
OCamlVisitor::TraverseArraySubscriptExpr (clang::ArraySubscriptExpr *S)
{
  TRACE;

  ptr<Expr> base = must_traverse (S->getBase ());
  ptr<Expr> idx = must_traverse (S->getIdx ());

  stack.push (mkArraySubscriptExpr (base, idx));

  return true;
}


bool
OCamlVisitor::TraverseStmtExpr (clang::StmtExpr *S)
{
  TRACE;

  ptr<Stmt> stmt = must_traverse (S->getSubStmt ());

  stack.push (mkStmtExpr (stmt));

  return true;
}


bool
OCamlVisitor::TraverseDeclRefExpr (clang::DeclRefExpr *S)
{
  TRACE;

  clang::StringRef name = getName (S);

  stack.push (mkDeclRefExpr (name));

  return true;
}


bool
OCamlVisitor::TraversePredefinedExpr (clang::PredefinedExpr *S)
{
  TRACE;

  PredefinedExpr kind = translate_predefined_expr (S->getIdentType ());

  stack.push (mkPredefinedExpr (kind));

  return true;
}


bool
OCamlVisitor::TraverseCStyleCastExpr (clang::CStyleCastExpr *S)
{
  TRACE;

  CastKind kind = translate_cast_kind (S->getCastKind ());
  ptr<Tloc> type = must_traverse (S->getTypeInfoAsWritten ()->getTypeLoc ());
  ptr<Expr> sub = must_traverse (S->getSubExpr ());

  stack.push (mkCStyleCastExpr (kind, type, sub));

  return true;
}


bool
OCamlVisitor::TraverseImplicitCastExpr (clang::ImplicitCastExpr *S)
{
  TRACE;

  CastKind kind = translate_cast_kind (S->getCastKind ());
  ptr<Expr> sub = must_traverse (S->getSubExpr ());

  stack.push (mkImplicitCastExpr (kind, sub));

  return true;
}


bool
OCamlVisitor::TraverseParenExpr (clang::ParenExpr *S)
{
  TRACE;

  ptr<Expr> sub = must_traverse (S->getSubExpr ());

  stack.push (mkParenExpr (sub));

  return true;
}


bool
OCamlVisitor::TraverseCompoundLiteralExpr (clang::CompoundLiteralExpr *S)
{
  TRACE;

  ptr<Tloc> type = getTypeLoc (S);
  ptr<Expr> init = must_traverse (S->getInitializer ());

  stack.push (mkCompoundLiteralExpr (type, init));

  return true;
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
  stack.push (designator);

  return true;
}


bool
OCamlVisitor::TraverseDesignatedInitExpr (clang::DesignatedInitExpr *S)
{
  TRACE;

  list<Desg> designators = traverse_list (designator_range (S), S);
  ptr<Expr> init = must_traverse (S->getInit ());

  stack.push (mkDesignatedInitExpr (designators, init));

  return true;
}


bool
OCamlVisitor::TraverseInitListExpr (clang::InitListExpr *S)
{
  TRACE;

  list<Expr> inits = traverse_list (S->children ());

  stack.push (mkInitListExpr (inits));

  return true;
}


bool
OCamlVisitor::TraverseVAArgExpr (clang::VAArgExpr *S)
{
  TRACE;

  ptr<Expr> sub = must_traverse (S->getSubExpr ());
  ptr<Tloc> type = must_traverse (S->getWrittenTypeInfo ()->getTypeLoc ());

  stack.push (mkVAArgExpr (sub, type));

  return true;
}


bool
OCamlVisitor::TraverseCallExpr (clang::CallExpr *S)
{
  TRACE;

  ptr<Expr> callee = must_traverse (S->getCallee ());
  list<Expr> args = traverse_list (arg_range (S));

  stack.push (mkCallExpr (callee, args));

  return true;
}


bool
OCamlVisitor::TraverseMemberExpr (clang::MemberExpr *S)
{
  TRACE;

  ptr<Expr> base = must_traverse (S->getBase ());
  clang::StringRef member = S->getMemberDecl ()->getName ();
  bool isArrow = S->isArrow ();

  stack.push (mkMemberExpr (base, member, isArrow));

  return true;
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
OCamlVisitor::TraverseUnaryExprOrTypeTraitExpr (clang::UnaryExprOrTypeTraitExpr *S)
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

  stack.push (mkAddrLabelExpr (label));

  return true;
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
      node = mkOON_Array (must_traverse (S->getIndexExpr (N.getArrayExprIndex ())));
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

  stack.push (node);

  return true;
}


bool
OCamlVisitor::TraverseOffsetOfExpr (clang::OffsetOfExpr *S)
{
  TRACE;

  ptr<Tloc> type = must_traverse (S->getTypeSourceInfo ()->getTypeLoc ());
  list<OffsetofNode> components = traverse_list (offsetof_node_range (S), S);

  stack.push (mkOffsetOfExpr (type, components));

  return true;
}


UNIMP_STMT (Expr, ArrayTypeTraitExpr)
UNIMP_STMT (Expr, AsTypeExpr)
UNIMP_STMT (Expr, AtomicExpr)
UNIMP_STMT (Expr, BinaryTypeTraitExpr)
UNIMP_STMT (Expr, BlockExpr)
UNIMP_STMT (Expr, ChooseExpr)
UNIMP_STMT (Expr, ConvertVectorExpr)
UNIMP_STMT (Expr, CUDAKernelCallExpr)
UNIMP_STMT (Expr, CXXBindTemporaryExpr)
UNIMP_STMT (Expr, CXXBoolLiteralExpr)
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

  stack.push (mkCXXNullPtrLiteralExpr ());

  return true;
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
UNIMP_STMT (Expr, ExtVectorElementExpr)
UNIMP_STMT (Expr, FunctionParmPackExpr)
UNIMP_STMT (Expr, GenericSelectionExpr)
UNIMP_STMT (Expr, GNUNullExpr)
UNIMP_STMT (Expr, LambdaExpr)
UNIMP_STMT (Expr, MaterializeTemporaryExpr)
UNIMP_STMT (Expr, MSPropertyRefExpr)
UNIMP_STMT (Expr, ObjCArrayLiteral)
UNIMP_STMT (Expr, ObjCBoolLiteralExpr)
UNIMP_STMT (Expr, ObjCBoxedExpr)
UNIMP_STMT (Expr, ObjCBridgedCastExpr)
UNIMP_STMT (Expr, ObjCDictionaryLiteral)
UNIMP_STMT (Expr, ObjCEncodeExpr)
UNIMP_STMT (Expr, ObjCIndirectCopyRestoreExpr)
UNIMP_STMT (Expr, ObjCIsaExpr)
UNIMP_STMT (Expr, ObjCIvarRefExpr)
UNIMP_STMT (Expr, ObjCMessageExpr)
UNIMP_STMT (Expr, ObjCPropertyRefExpr)
UNIMP_STMT (Expr, ObjCProtocolExpr)
UNIMP_STMT (Expr, ObjCSelectorExpr)
UNIMP_STMT (Expr, ObjCStringLiteral)
UNIMP_STMT (Expr, ObjCSubscriptRefExpr)
UNIMP_STMT (Expr, PackExpansionExpr)
UNIMP_STMT (Expr, ParenListExpr)
UNIMP_STMT (Expr, PseudoObjectExpr)
UNIMP_STMT (Expr, ShuffleVectorExpr)
UNIMP_STMT (Expr, SizeOfPackExpr)
UNIMP_STMT (Expr, SubstNonTypeTemplateParmExpr)
UNIMP_STMT (Expr, SubstNonTypeTemplateParmPackExpr)
UNIMP_STMT (Expr, TypeTraitExpr)
UNIMP_STMT (Expr, UnaryTypeTraitExpr)
UNIMP_STMT (Expr, UnresolvedLookupExpr)
UNIMP_STMT (Expr, UnresolvedMemberExpr)
UNIMP_STMT (Expr, UserDefinedLiteral)


// }}}
