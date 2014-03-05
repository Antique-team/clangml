open ClangAst

(* Removes implicit casts and parens from expressions. *)

let rec strip_designator desg =
  match desg.dr with
  | FieldDesignator _ -> desg
  | ArrayDesignator expr ->
      { desg with dr = ArrayDesignator (strip_expr expr) }
  | ArrayRangeDesignator (e1, e2) ->
      { desg with dr = ArrayRangeDesignator (strip_expr e1, strip_expr e2) }


and strip_expr_ = function
  | IntegerLiteral _
  | CharacterLiteral _
  | FloatingLiteral _
  | StringLiteral _
  | DeclRefExpr _
  | PredefinedExpr _
  | ImplicitValueInitExpr
  | SizeOfType _
  | AlignOfType _
  | VecStepType _
  | UnimpExpr _ as e -> e

  | BinaryOperator (op, e1, e2) ->
      BinaryOperator (op, strip_expr e1, strip_expr e2)
  | UnaryOperator (op, e1) ->
      UnaryOperator (op, strip_expr e1)

  | CStyleCastExpr (kind, ty, expr) ->
      CStyleCastExpr (kind, ty, strip_expr expr)
  | CompoundLiteralExpr (ty, expr) ->
      CompoundLiteralExpr (ty, strip_expr expr)
  | VAArgExpr (expr, ty) ->
      VAArgExpr (strip_expr expr, ty)
  | CallExpr (callee, args) ->
      CallExpr (strip_expr callee, List.map strip_expr args)
  | MemberExpr (base, member, is_arrow) ->
      MemberExpr (strip_expr base, member, is_arrow)
  | ConditionalOperator (e1, e2, e3) ->
      ConditionalOperator (strip_expr e1, strip_expr e2,
                           strip_expr e3)
  | DesignatedInitExpr (designators, init) ->
      DesignatedInitExpr (List.map strip_designator designators,
                          strip_expr init)
  | InitListExpr inits ->
      InitListExpr (List.map strip_expr inits)
  | ArraySubscriptExpr (base, index) ->
      ArraySubscriptExpr (strip_expr base, strip_expr index)
  | StmtExpr _ -> failwith "untyper does not yet support StmtExpr"

  | SizeOfExpr expr ->
      SizeOfExpr (strip_expr expr)
  | AlignOfExpr expr ->
      AlignOfExpr (strip_expr expr)
  | VecStepExpr expr ->
      VecStepExpr (strip_expr expr)

  (* These should not occur, anymore,
     since strip_expr took care of them. *)
  | ParenExpr _ | ImplicitCastExpr _ -> assert false


and strip_expr expr =
  match expr.e with
  | ParenExpr e1
  | ImplicitCastExpr (_, e1) -> strip_expr e1

  | _ ->
      let e = strip_expr_ expr.e in
      if e == expr.e then
        (* Nothing changed. *)
        expr
      else
        { expr with e }
