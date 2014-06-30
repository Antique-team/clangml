open Clang
open Util.Prelude


let transform_decl clang =
  let open Ast in

  let rec map_stmt v state stmt =
    match stmt.s with
    | CompoundStmt stmts ->
        MapStmt.mapCompoundStmt map_stmt v state stmt stmts

    | ForStmt (init, cond, incr, body) ->
        let (state, init) = Visitor.map_option map_stmt v state init in
        let (state, cond) = Visitor.map_option MapVisitor.visit_expr v state cond in
        let (state, incr) = Visitor.map_option MapVisitor.visit_expr v state incr in
        let (state, body) = map_stmt v state body in

        let init = Util.Option.to_list init in
        let incr =
          Util.Option.map (fun incr ->
              { s = ExprStmt incr;
                s_sloc = incr.e_sloc;
                s_cref = Ref.null;
              }
            ) incr
          |> Util.Option.to_list
        in

        let cond =
          Util.Option.default {
            e = IntegerLiteral 1;
            e_type = {
              t        = BuiltinType BT_Int;
              t_qual   = [];
              t_aspace = None;
              t_cref   = Ref.null;
              t_self   = Util.DenseIntMap.null_key;
              t_canon  = Util.DenseIntMap.null_key;
            };
            e_sloc = stmt.s_sloc;
            e_cref = Ref.null;
          } cond
        in

        let body = {
          s = CompoundStmt (body :: incr);
          s_sloc = body.s_sloc;
          s_cref = Ref.null;
        } in

        let state =
          [
            { s = WhileStmt (cond, body);
              s_sloc = stmt.s_sloc;
              s_cref = Ref.null;
            };
          ]
          @ init
        in

        (state, stmt)

    | _ ->
        MapVisitor.visit_stmt v state stmt

  in

  let v = MapVisitor.({ default with map_stmt }) in

  snd % MapVisitor.visit_decl v []
