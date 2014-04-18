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
#define STMT(CLASS, PARENT)				\
    case clang::Stmt::CLASS##Class:			\
      Traverse##CLASS (static_cast<clang::CLASS *> (S));\
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

  stack.push (mkNullStmt ());

  return true;
}


bool
OCamlVisitor::TraverseBreakStmt (clang::BreakStmt *S)
{
  TRACE;

  stack.push (mkBreakStmt ());

  return true;
}


bool
OCamlVisitor::TraverseContinueStmt (clang::ContinueStmt *S)
{
  TRACE;

  stack.push (mkContinueStmt ());

  return true;
}


bool
OCamlVisitor::TraverseLabelStmt (clang::LabelStmt *S)
{
  TRACE;

  clang::StringRef name = S->getDecl ()->getName ();
  ptr<Stmt> sub = must_traverse (S->getSubStmt ());

  stack.push (mkLabelStmt (name, sub));

  return true;
}


bool
OCamlVisitor::TraverseCaseStmt (clang::CaseStmt *S)
{
  TRACE;

  ptr<Expr> lhs = must_traverse (S->getLHS ());
  option<Expr> rhs = maybe_traverse (S->getRHS ());
  ptr<Stmt> sub = must_traverse (S->getSubStmt ());

  stack.push (mkCaseStmt (lhs, rhs, sub));

  return true;
}


bool
OCamlVisitor::TraverseDefaultStmt (clang::DefaultStmt *S)
{
  TRACE;

  ptr<Stmt> sub = must_traverse (S->getSubStmt ());

  stack.push (mkDefaultStmt (sub));

  return true;
}


bool
OCamlVisitor::TraverseGotoStmt (clang::GotoStmt *S)
{
  TRACE;

  clang::StringRef name = S->getLabel ()->getName ();

  stack.push (mkGotoStmt (name));

  return true;
}


bool
OCamlVisitor::TraverseIfStmt (clang::IfStmt *S)
{
  TRACE;

  ptr<Expr> cond = must_traverse (S->getCond ());
  ptr<Stmt> thenBranch = must_traverse (S->getThen ());
  option<Stmt> elseBranch = maybe_traverse (S->getElse ());

  stack.push (mkIfStmt (cond, thenBranch, elseBranch));

  return true;
}


bool
OCamlVisitor::TraverseForStmt (clang::ForStmt *S)
{
  TRACE;

  option<Stmt> init = maybe_traverse (S->getInit ());
  option<Expr> cond = maybe_traverse (S->getCond ());
  option<Expr> inc = maybe_traverse (S->getInc ());
  ptr<Stmt> body = must_traverse (S->getBody ());

  stack.push (mkForStmt (init, cond, inc, body));

  return true;
}


bool
OCamlVisitor::TraverseWhileStmt (clang::WhileStmt *S)
{
  TRACE;

  ptr<Expr> cond = maybe_traverse (S->getCond ());
  ptr<Stmt> body = must_traverse (S->getBody ());

  stack.push (mkWhileStmt (cond, body));

  return true;
}


bool
OCamlVisitor::TraverseDoStmt (clang::DoStmt *S)
{
  TRACE;

  ptr<Stmt> body = must_traverse (S->getBody ());
  ptr<Expr> cond = maybe_traverse (S->getCond ());

  stack.push (mkDoStmt (body, cond));

  return true;
}


bool
OCamlVisitor::TraverseSwitchStmt (clang::SwitchStmt *S)
{
  TRACE;

  ptr<Expr> cond = must_traverse (S->getCond ());
  ptr<Stmt> body = must_traverse (S->getBody ());

  stack.push (mkSwitchStmt (cond, body));

  return true;
}


bool
OCamlVisitor::TraverseReturnStmt (clang::ReturnStmt *S)
{
  TRACE;

  option<Expr> expr = maybe_traverse (S->getRetValue ());

  stack.push (mkReturnStmt (expr));

  return true;
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

  stack.push (mkCompoundStmt (stmts));

  return true;
}


bool
OCamlVisitor::TraverseGCCAsmStmt (clang::GCCAsmStmt *S)
{
  TRACE;

  // TODO: implement
  stack.push (ast_bridge::mkGCCAsmStmt ());

  return true;
}


bool
OCamlVisitor::TraverseDeclStmt (clang::DeclStmt *S)
{
  TRACE;

  list<Decl> decls = traverse_list (decl_range (S));

  stack.push (mkDeclStmt (decls));

  return true;
}


UNIMP_STMT (Stmt, AttributedStmt)
UNIMP_STMT (Stmt, CapturedStmt)
UNIMP_STMT (Stmt, CXXCatchStmt)
UNIMP_STMT (Stmt, CXXForRangeStmt)
UNIMP_STMT (Stmt, CXXTryStmt)
UNIMP_STMT (Stmt, IndirectGotoStmt)
UNIMP_STMT (Stmt, MSAsmStmt)
UNIMP_STMT (Stmt, MSDependentExistsStmt)
UNIMP_STMT (Stmt, ObjCAtCatchStmt)
UNIMP_STMT (Stmt, ObjCAtFinallyStmt)
UNIMP_STMT (Stmt, ObjCAtSynchronizedStmt)
UNIMP_STMT (Stmt, ObjCAtThrowStmt)
UNIMP_STMT (Stmt, ObjCAtTryStmt)
UNIMP_STMT (Stmt, ObjCAutoreleasePoolStmt)
UNIMP_STMT (Stmt, ObjCForCollectionStmt)
UNIMP_STMT (Stmt, OMPParallelDirective)
UNIMP_STMT (Stmt, SEHExceptStmt)
UNIMP_STMT (Stmt, SEHFinallyStmt)
UNIMP_STMT (Stmt, SEHTryStmt)


// }}}
