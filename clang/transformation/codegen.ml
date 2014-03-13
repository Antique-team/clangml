open Clang


let declare sloc var ty =
  let open Ast in

  {
    s = DeclStmt [{
        d = VarDecl (
            ty,
            var,
            None
          );
        d_sloc = sloc;
        d_cref = Ref.null;
      }];
    s_sloc = sloc;
    s_cref = Ref.null;
  }


let assign var expr =
  let open Ast in

  { s = ExprStmt {
        e = BinaryOperator (
            BO_Assign,
            { e = DeclRefExpr var;
              e_sloc = expr.e_sloc;
              e_type = expr.e_type;
              e_cref = Ref.null;
            },
            expr
          );
        e_sloc = expr.e_sloc;
        e_type = expr.e_type;
        e_cref = Ref.null;
      };
    s_sloc = expr.e_sloc;
    s_cref = Ref.null;
  }


let append_stmts stmt stmts =
  let open Ast in

  match stmts with
  | [] -> stmt
  | stmts ->
      { s = CompoundStmt (stmt :: stmts);
        s_sloc = stmt.s_sloc;
        s_cref = Ref.null;
      }
