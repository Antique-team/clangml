open Clang
open Util.Prelude

let generated_vars = ref 0

let create_tmp_var () =
  let var = "%post_incr_decr_tmp" ^ string_of_int !generated_vars in
  incr generated_vars;
  var

let transform_decl clang =
  let open Ast in

  let rec map_stmt v state stmt =
    match stmt.s with
    | CompoundStmt stmts ->
        MapStmt.mapCompoundStmt ~replace:false map_stmt v state stmt stmts
    | _ ->
        MapVisitor.visit_stmt v state stmt

  and map_expr v state expr =

    match expr.e with
      | UnaryOperator ((UO_PostInc | UO_PostDec) as op, operand) ->
          (* a++ becomes
             tmp = a;
             ++a *)
          (* store current a in a tmp var *)
          let tmp_var = create_tmp_var () in
          let decl_tmp_var =
            Codegen.declare ~init:operand operand.e_sloc tmp_var
              (Types.tloc_of_ctyp expr.e_sloc expr.e_type)
          in
          let state' = decl_tmp_var :: state in
          (* we reuse the previous implementation of {++/--}a to do the rest *)
          let final_op = match op with
            | UO_PostInc -> UO_PreInc
            | UO_PostDec -> UO_PreDec
            | _ -> assert(false);
          in
          let new_expr =
            { s = ExprStmt {
                expr with e = UnaryOperator (final_op, operand)
               };
              s_sloc = expr.e_sloc;
              s_cref = Ref.null;
            }
          in
          let state'' = new_expr :: state' in
          let expr_to_return =
            { e = DeclRefExpr tmp_var;
              e_sloc = expr.e_sloc;
              e_type = expr.e_type;
              e_cref = Ref.null;
            }
          in
          map_expr v state'' expr_to_return
      | _ ->
          MapVisitor.visit_expr v state expr

  in

  let v = MapVisitor.({ default with map_expr; map_stmt }) in

  snd % MapVisitor.visit_decl v []
