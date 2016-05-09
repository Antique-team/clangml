#include "OCamlVisitor.h"


/****************************************************
 * {{{1 Statements
 */


bool
OCamlVisitor::TraverseStmt (clang::Stmt *S)
{
  // Top switch stmt: dispatch to TraverseFooStmt for each concrete FooStmt.
  switch (S->getStmtClass ())
    {
    case clang::Stmt::NoStmtClass: break;
#define ABSTRACT_STMT(STMT)
#define STMT(CLASS, PARENT)                                     \
      case clang::Stmt::CLASS##Class:                           \
        Traverse##CLASS (static_cast<clang::CLASS *> (S));      \
        break;
#include <clang/AST/StmtNodes.inc>
    }

  if (clang::Expr *E = clang::dyn_cast<clang::Expr> (S))
    {
      ptr<Expr> expr = mkExpr ();
      expr->e      = stack.pop ();
      expr->e_cref = ref (E);
      expr->e_sloc = sloc (E);
      expr->e_type = must_traverse (E->getType ());
      stack.push (expr);
    }
  else
    {
      ptr<Stmt> stmt = mkStmt ();
      stmt->s      = stack.pop ();
      stmt->s_cref = ref (S);
      stmt->s_sloc = sloc (S);
      stack.push (stmt);
    }

  return true;
}


bool
OCamlVisitor::TraverseNullStmt (clang::NullStmt *S)
{
  TRACE;

  return stack.push (mkNullStmt ());
}


bool
OCamlVisitor::TraverseBreakStmt (clang::BreakStmt *S)
{
  TRACE;

  return stack.push (mkBreakStmt ());
}


bool
OCamlVisitor::TraverseContinueStmt (clang::ContinueStmt *S)
{
  TRACE;

  return stack.push (mkContinueStmt ());
}


bool
OCamlVisitor::TraverseLabelStmt (clang::LabelStmt *S)
{
  TRACE;

  clang::StringRef name = S->getDecl ()->getName ();
  ptr<Stmt> sub = must_traverse (S->getSubStmt ());

  return stack.push (mkLabelStmt (name, sub));
}


bool
OCamlVisitor::TraverseCaseStmt (clang::CaseStmt *S)
{
  TRACE;

  ptr<Expr> lhs = must_traverse (S->getLHS ());
  option<Expr> rhs = maybe_traverse (S->getRHS ());
  ptr<Stmt> sub = must_traverse (S->getSubStmt ());

  return stack.push (mkCaseStmt (lhs, rhs, sub));
}


bool
OCamlVisitor::TraverseDefaultStmt (clang::DefaultStmt *S)
{
  TRACE;

  ptr<Stmt> sub = must_traverse (S->getSubStmt ());

  return stack.push (mkDefaultStmt (sub));
}


bool
OCamlVisitor::TraverseGotoStmt (clang::GotoStmt *S)
{
  TRACE;

  clang::StringRef name = S->getLabel ()->getName ();

  return stack.push (mkGotoStmt (name));
}


bool
OCamlVisitor::TraverseIfStmt (clang::IfStmt *S)
{
  TRACE;

  ptr<Expr> cond = must_traverse (S->getCond ());
  ptr<Stmt> thenBranch = must_traverse (S->getThen ());
  option<Stmt> elseBranch = maybe_traverse (S->getElse ());

  return stack.push (mkIfStmt (cond, thenBranch, elseBranch));
}


bool
OCamlVisitor::TraverseForStmt (clang::ForStmt *S)
{
  TRACE;

  option<Stmt> init = maybe_traverse (S->getInit ());
  option<Expr> cond = maybe_traverse (S->getCond ());
  option<Expr> inc = maybe_traverse (S->getInc ());
  ptr<Stmt> body = must_traverse (S->getBody ());

  return stack.push (mkForStmt (init, cond, inc, body));
}


bool
OCamlVisitor::TraverseWhileStmt (clang::WhileStmt *S)
{
  TRACE;

  ptr<Expr> cond = must_traverse (S->getCond ());
  ptr<Stmt> body = must_traverse (S->getBody ());

  return stack.push (mkWhileStmt (cond, body));
}


bool
OCamlVisitor::TraverseDoStmt (clang::DoStmt *S)
{
  TRACE;

  ptr<Stmt> body = must_traverse (S->getBody ());
  ptr<Expr> cond = must_traverse (S->getCond ());

  return stack.push (mkDoStmt (body, cond));
}


bool
OCamlVisitor::TraverseSwitchStmt (clang::SwitchStmt *S)
{
  TRACE;

  ptr<Expr> cond = must_traverse (S->getCond ());
  ptr<Stmt> body = must_traverse (S->getBody ());

  return stack.push (mkSwitchStmt (cond, body));
}


bool
OCamlVisitor::TraverseReturnStmt (clang::ReturnStmt *S)
{
  TRACE;

  option<Expr> expr = maybe_traverse (S->getRetValue ());

  return stack.push (mkReturnStmt (expr));
}


bool
OCamlVisitor::TraverseCompoundStmt (clang::CompoundStmt *S)
{
  TRACE;

  stack.push_mark ();
  for (clang::Stmt *sub : S->children ())
    // Traverse, but do not pop yet.
    traverse (sub);
  list<Stmt> stmts = stack.pop_marked ();

  return stack.push (mkCompoundStmt (stmts));
}


bool
OCamlVisitor::TraverseGCCAsmStmt (clang::GCCAsmStmt *S)
{
  TRACE;

  // asm instruction(s)
  ptr<Expr> asm_string = must_traverse (S->getAsmString ());

  // outputs
  list<AsmArg> outputs;
  for (unsigned int i = 0; i < S->getNumOutputs (); i++)
    {
      ptr<AsmArg> output = mkAsmArg ();
      output->aa_constraint = S->getOutputConstraint (i);
      output->aa_expr = must_traverse (S->getOutputExpr (i));
      outputs.push_back (output);
    }

  // inputs
  list<AsmArg> inputs;
  for (unsigned int i = 0; i < S->getNumInputs (); i++)
    {
      ptr<AsmArg> input = mkAsmArg ();
      input->aa_constraint = S->getInputConstraint (i);
      input->aa_expr = must_traverse (S->getInputExpr (i));
      inputs.push_back (input);
    }

  // clobbers
  std::vector<clang::StringRef> clobbers;
  for (unsigned int i = 0; i < S->getNumClobbers (); i++)
    clobbers.push_back (S->getClobber (i));

  return stack.push
    (ast_bridge::mkGCCAsmStmt (asm_string, outputs, inputs, clobbers));
}


bool
OCamlVisitor::TraverseDeclStmt (clang::DeclStmt *S)
{
  TRACE;

  list<Decl> decls = traverse_list (decl_range (S));

  return stack.push (mkDeclStmt (decls));
}


bool
OCamlVisitor::TraverseIndirectGotoStmt (clang::IndirectGotoStmt *S)
{
  TRACE;

  ptr<Expr> expr = must_traverse (S->getTarget ());

  return stack.push (mkIndirectGotoStmt (expr));
}

bool
OCamlVisitor::TraverseCapturedStmt (clang::CapturedStmt *S)
{
  TRACE;

  CapturedRegionKind kind =
    translate_captured_region_kind(S->getCapturedRegionKind ());
  ptr<Stmt> stmt = must_traverse (S->getCapturedStmt ());
  ptr<Decl> decl = must_traverse (S->getCapturedDecl ());
  list<Stmt> captures = traverse_list (S->children());

  return stack.push (mkCapturedStmt (kind, stmt, decl, captures));
}

bool
OCamlVisitor::TraverseObjCAtFinallyStmt (clang::ObjCAtFinallyStmt *S)
{
  TRACE;

  ptr<Stmt> body = must_traverse (S->getFinallyBody ());

  return stack.push (mkObjCAtFinallyStmt (body));
}

bool
OCamlVisitor::TraverseObjCAtTryStmt (clang::ObjCAtTryStmt *S)
{
  TRACE;

  ptr<Stmt> try_body = must_traverse (S->getTryBody ());

  list<Stmt> catch_stmts;
  for (unsigned i = 0; i < S->getNumCatchStmts (); ++i)
    {
      catch_stmts.push_back(must_traverse (S->getCatchStmt (i)));
    }

  option<Stmt> finally_body = maybe_traverse (S->getFinallyStmt ());

  return stack.push (mkObjCAtTryStmt (try_body, catch_stmts, finally_body));
}

bool
OCamlVisitor::TraverseObjCAtCatchStmt (clang::ObjCAtCatchStmt *S)
{
  TRACE;

  ptr<Decl> param = must_traverse (S->getCatchParamDecl ());
  ptr<Stmt> body = must_traverse (S->getCatchBody ());

  return stack.push (mkObjCAtCatchStmt (param, body));
}

bool
OCamlVisitor::TraverseObjCAtThrowStmt (clang::ObjCAtThrowStmt *S)
{
  TRACE;

  ptr<Expr> throw_expr = must_traverse (S->getThrowExpr ());

  return stack.push (mkObjCAtThrowStmt (throw_expr));
}

bool
OCamlVisitor::TraverseObjCAtSynchronizedStmt (clang::ObjCAtSynchronizedStmt *S)
{
  TRACE;

  ptr<Expr> expr = must_traverse (S->getSynchExpr ());
  list<Stmt> body = traverse_list (S->getSynchBody ()->children());

  return stack.push (mkObjCAtSynchronizedStmt (expr, body));
}

bool
OCamlVisitor::TraverseObjCForCollectionStmt (clang::ObjCForCollectionStmt *S)
{
  TRACE;

  ptr<Stmt> element = must_traverse (S-> getElement());
  ptr<Expr> collection = must_traverse (S-> getCollection());
  ptr<Stmt> body = must_traverse (S-> getBody());

  return stack.push (mkObjCForCollectionStmt (element, collection, body));
}


UNIMP_STMT (Stmt, AttributedStmt)
UNIMP_STMT (Stmt, CXXCatchStmt)
UNIMP_STMT (Stmt, CXXForRangeStmt)
UNIMP_STMT (Stmt, CXXTryStmt)
UNIMP_STMT (Stmt, CoreturnStmt)
UNIMP_STMT (Stmt, CoroutineBodyStmt)
UNIMP_STMT (Stmt, MSAsmStmt)
UNIMP_STMT (Stmt, MSDependentExistsStmt)
UNIMP_STMT (Stmt, OMPAtomicDirective)
UNIMP_STMT (Stmt, OMPBarrierDirective)
UNIMP_STMT (Stmt, OMPCancelDirective)
UNIMP_STMT (Stmt, OMPCancellationPointDirective)
UNIMP_STMT (Stmt, OMPCriticalDirective)
UNIMP_STMT (Stmt, OMPDistributeDirective)
UNIMP_STMT (Stmt, OMPFlushDirective)
UNIMP_STMT (Stmt, OMPForDirective)
UNIMP_STMT (Stmt, OMPForSimdDirective)
UNIMP_STMT (Stmt, OMPMasterDirective)
UNIMP_STMT (Stmt, OMPOrderedDirective)
UNIMP_STMT (Stmt, OMPParallelDirective)
UNIMP_STMT (Stmt, OMPParallelForDirective)
UNIMP_STMT (Stmt, OMPParallelForSimdDirective)
UNIMP_STMT (Stmt, OMPParallelSectionsDirective)
UNIMP_STMT (Stmt, OMPSectionDirective)
UNIMP_STMT (Stmt, OMPSectionsDirective)
UNIMP_STMT (Stmt, OMPSimdDirective)
UNIMP_STMT (Stmt, OMPSingleDirective)
UNIMP_STMT (Stmt, OMPTargetDataDirective)
UNIMP_STMT (Stmt, OMPTargetDirective)
UNIMP_STMT (Stmt, OMPTaskDirective)
UNIMP_STMT (Stmt, OMPTaskLoopDirective)
UNIMP_STMT (Stmt, OMPTaskLoopSimdDirective)
UNIMP_STMT (Stmt, OMPTaskgroupDirective)
UNIMP_STMT (Stmt, OMPTaskwaitDirective)
UNIMP_STMT (Stmt, OMPTaskyieldDirective)
UNIMP_STMT (Stmt, OMPTeamsDirective)
UNIMP_STMT (Stmt, ObjCAutoreleasePoolStmt)
UNIMP_STMT (Stmt, SEHExceptStmt)
UNIMP_STMT (Stmt, SEHFinallyStmt)
UNIMP_STMT (Stmt, SEHLeaveStmt)
UNIMP_STMT (Stmt, SEHTryStmt)


// }}}
