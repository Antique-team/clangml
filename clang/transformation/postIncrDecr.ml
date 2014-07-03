open Clang
open Util.Prelude

let transform_decl clang =
  let open Ast in

  let rec map_expr v state expr =

    match expr.e with
      | UnaryOperator ((UO_PostInc | UO_PostDec) as op, operand) ->
          (* a++ becomes
             tmp = a;
             ++a *)
          let final_op = match op with
            | UO_PostInc -> UO_PreInc
            | UO_PostDec -> UO_PreDec
            | _ -> assert(false);
          in
          let new_expr = { expr with e = UnaryOperator (final_op, operand) } in
          map_expr v state new_expr
      | _ ->
          MapVisitor.visit_expr v state expr

  in

  let v = MapVisitor.({ default with map_expr }) in

  snd % MapVisitor.visit_decl v []
