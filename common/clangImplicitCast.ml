open ClangAst

(* Removes types, implicit casts, and parens from expressions. *)

let rec strip_designator = function
  | FieldDesignator _ as d -> d
  | ArrayDesignator (sloc, expr) ->
      ArrayDesignator (sloc, strip_expr expr)
  | ArrayRangeDesignator (sloc, e1, e2) ->
      ArrayRangeDesignator (sloc, strip_expr e1, strip_expr e2)


and strip_expr = function
  | ParenExpr (_, TypedExpr (e, _))
  | ImplicitCastExpr (_, TypedExpr (e, _)) -> strip_expr e
  | ParenExpr (_, e)
  | ImplicitCastExpr (_, e) -> strip_expr e

  | IntegerLiteral _
  | CharacterLiteral _
  | FloatingLiteral _
  | StringLiteral _
  | DeclRefExpr _
  | PredefinedExpr _
  | ImplicitValueInitExpr _
  | SizeOfType _
  | AlignOfType _
  | VecStepType _
  | UnimpExpr _ as e -> e

  | TypedExpr (e, ty) ->
      TypedExpr (strip_expr e, ty)
  | BinaryOperator (sloc, op, e1, e2) ->
      BinaryOperator (sloc, op, strip_expr e1, strip_expr e2)
  | UnaryOperator (sloc, op, e) ->
      UnaryOperator (sloc, op, strip_expr e)

  | CStyleCastExpr (sloc, ty, expr) ->
      CStyleCastExpr (sloc, ty, strip_expr expr)
  | CompoundLiteralExpr (sloc, ty, expr) ->
      CompoundLiteralExpr (sloc, ty, strip_expr expr)
  | VAArgExpr (sloc, expr, ty) ->
      VAArgExpr (sloc, strip_expr expr, ty)
  | CallExpr (sloc, callee, args) ->
      CallExpr (sloc, strip_expr callee, List.map strip_expr args)
  | MemberExpr (sloc, base, member, is_arrow) ->
      MemberExpr (sloc, strip_expr base, member, is_arrow)
  | ConditionalOperator (sloc, e1, e2, e3) ->
      ConditionalOperator (sloc, strip_expr e1, strip_expr e2, strip_expr e3)
  | DesignatedInitExpr (sloc, designators, init) ->
      DesignatedInitExpr (sloc, List.map strip_designator designators,
                          strip_expr init)
  | InitListExpr (sloc, inits) ->
      InitListExpr (sloc, List.map strip_expr inits)
  | ArraySubscriptExpr (sloc, base, index) ->
      ArraySubscriptExpr (sloc, strip_expr base, strip_expr index)
  | StmtExpr _ -> failwith "untyper does not yet support StmtExpr"

  | SizeOfExpr (sloc, expr) ->
      SizeOfExpr (sloc, strip_expr expr)
  | AlignOfExpr (sloc, expr) ->
      AlignOfExpr (sloc, strip_expr expr)
  | VecStepExpr (sloc, expr) ->
      VecStepExpr (sloc, strip_expr expr)
