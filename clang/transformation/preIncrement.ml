open Clang
open Util.Prelude

let transform_decl clang =
  let open Ast in

  let rec map_expr v state expr =

    match expr.e with
      | UnaryOperator (UO_PreInc, operand) ->
          (* ++a becomes a = a + 1 *)
          let one_lit =
            { e = IntegerLiteral 1 ;
              e_cref = Ref.null ;
              e_sloc = expr.e_sloc ;
              e_type = operand.e_type (* FIXME THIS SHOULD BE INTEGER *)
            } in
          let a_plus_one =
            { e = BinaryOperator (BO_Add, operand, one_lit) ;
              e_cref = Ref.null ;
              e_sloc = expr.e_sloc ;
              e_type = operand.e_type
            } in
          let store_a_plus_one = 
            { e = BinaryOperator (BO_Assign, operand, a_plus_one) ;
              e_cref = Ref.null ;
              e_sloc = expr.e_sloc ;
              e_type = operand.e_type
            } in
          map_expr v state store_a_plus_one
      | _ ->
          MapVisitor.visit_expr v state expr

  in

  let v = MapVisitor.({ default with map_expr }) in

  snd % MapVisitor.visit_decl v []
