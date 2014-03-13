open Clang
open Prelude


let transform_decl clang =
  let open Ast in

  let rec map_stmt v state stmt =
    match stmt.s with
    | CompoundStmt stmts ->
        MapStmt.mapCompoundStmt map_stmt v state stmt stmts

    | DeclStmt [{ d = VarDecl (ty, name, Some init) } as decl] ->
        let init_stmt = {
          s = ExprStmt {
              e = BinaryOperator (
                  BO_Assign,
                  { e = DeclRefExpr name;
                    e_sloc = decl.d_sloc;
                    e_type = Api.(request clang @@ TypePtr ty.tl_cref);
                    e_cref = Ref.null;
                  },
                  init
                );
              e_sloc = decl.d_sloc;
              e_type = init.e_type;
              e_cref = Ref.null;
            };
          s_sloc = init.e_sloc;
          s_cref = Ref.null;
        } in

        let state = [
          init_stmt;
          { stmt with
            s = DeclStmt [{ decl with d = VarDecl (ty, name, None) }];
          };
        ] in

        (state, stmt)

    | DeclStmt (_::_::_) ->
        failwith "SplitInitialisers cannot handle multiple declarations"

    | _ ->
        MapVisitor.visit_stmt v state stmt

  in

  let v = MapVisitor.({ default with map_stmt }) in

  snd % MapVisitor.visit_decl v []
