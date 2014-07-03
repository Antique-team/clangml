open Clang
open Util.Prelude


let transform_decl clang =
  let open Ast in

  let rec map_expr v state expr =
    match expr.e with
    | ParenExpr e
    | ImplicitCastExpr (_, e) ->
        (* Assign the conversion type to the subexpression.
           E.g. this changes the type of [( void * )0] in
             [int *a = (( void * )0);]
           from [void *] to [int *].
        *)
        map_expr v state { e with e_type = expr.e_type }

    | _ ->
        MapVisitor.visit_expr v state expr

  in

  let v = MapVisitor.({ default with map_expr }) in

  snd % MapVisitor.visit_decl v []
