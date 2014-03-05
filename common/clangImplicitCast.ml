open ClangAst

(* Removes implicit casts and parens from expressions. *)

let rec strip_designator dr =
  match dr.dr with
  | FieldDesignator _ -> dr
  | ArrayDesignator (expr) ->
      { dr with dr = ArrayDesignator (strip_expr expr) }
  | ArrayRangeDesignator (e1, e2) ->
      { dr with dr = ArrayRangeDesignator (strip_expr e1, strip_expr e2) }


and strip_expr e =
  match e.e with
  | ParenExpr (e1)
  | ImplicitCastExpr (e1) -> strip_expr e1

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
  | UnimpExpr _ -> e

  | BinaryOperator (op, e1, e2) ->
      { e with e = BinaryOperator (op, strip_expr e1, strip_expr e2) }
  | UnaryOperator (op, e1) ->
      { e with e = UnaryOperator (op, strip_expr e1) }

  | CStyleCastExpr (ty, expr) ->
      { e with e = CStyleCastExpr (ty, strip_expr expr) }
  | CompoundLiteralExpr (ty, expr) ->
      { e with e = CompoundLiteralExpr (ty, strip_expr expr) }
  | VAArgExpr (expr, ty) ->
      { e with e = VAArgExpr (strip_expr expr, ty) }
  | CallExpr (callee, args) ->
      { e with e = CallExpr (strip_expr callee, List.map strip_expr args)
      }
  | MemberExpr (base, member, is_arrow) ->
      { e with e = MemberExpr (strip_expr base, member, is_arrow) }
  | ConditionalOperator (e1, e2, e3) ->
      { e with e = ConditionalOperator (strip_expr e1, strip_expr e2,
                                        strip_expr e3) }
  | DesignatedInitExpr (designators, init) ->
      { e with e = DesignatedInitExpr (List.map strip_designator designators,
                                       strip_expr init) }
  | InitListExpr (inits) ->
      { e with e = InitListExpr (List.map strip_expr inits) }
  | ArraySubscriptExpr (base, index) ->
      { e with e = ArraySubscriptExpr (strip_expr base, strip_expr index) }
  | StmtExpr _ -> failwith "untyper does not yet support StmtExpr"

  | SizeOfExpr (expr) ->
      { e with e = SizeOfExpr (strip_expr expr) }
  | AlignOfExpr (expr) ->
      { e with e = AlignOfExpr (strip_expr expr) }
  | VecStepExpr (expr) ->
      { e with e = VecStepExpr (strip_expr expr) }
