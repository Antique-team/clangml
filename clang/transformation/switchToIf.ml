open Clang
open Util.Prelude


let make_temporary vars =
  let var = "%tmp" ^ string_of_int vars in
  (vars + 1, var)


type cases = {
  (* The default case does not need an expression list.
     Even if there is another case pointing at the
     default, its argument is irrelevant. *)
  default : Ast.stmt list;
  (* List of expressions with a statement. E.g. in
       case 1:
       case 2:
         return 0;
     we have the tuple
       ([1; 2], return 0)
  *)
  groups : ((Ast.expr * Ast.expr option) list * Ast.stmt) list
}


type case =
  | Default
  | Case of (Ast.expr * Ast.expr option) list
  deriving (Show)

let rec collect_cases cases = let open Ast in function
  | { s = CaseStmt (l, r, body) } ->
      collect_cases (
        match cases with
        | Default -> Default
        | Case list -> Case ((l, r) :: list)
      ) body

  | { s = DefaultStmt body } ->
      collect_cases Default body

  | body ->
      cases, body
  

let transform_decl clang =
  let open Ast in

  let rec map_stmt v vars stmt =
    match stmt.s with
    | SwitchStmt (cond, cases) ->
        let (vars, cond) = MapVisitor.visit_expr v vars cond in
        let (var, cases) =
          let (vars, var) = make_temporary vars in
          let (vars, cases) = map_stmt v vars cases in
          (var, cases)
        in

        let cases =
          match cases.s with
          | CompoundStmt [] ->
              failwith "switch statement body must have at least one case"
          | CompoundStmt cases ->
              List.map (collect_cases (Case [])) cases
          | _ -> failwith "switch statement body must be CompoundStmt"
        in

        (* Get the default case to the end. *)
        let cases = List.sort (fun a b ->
            match fst a, fst b with
            | Case _, Case _
            | Default, Default ->  0
            | Case _, Default  -> -1
            | Default, Case _  ->  1
          ) cases
        in

        print_endline @@ Show.show<case list> @@ fst @@ List.split cases;

        (*
        let cases =
          match cases.s with
          | CompoundStmt (case0 :: cases) ->
              List.fold_left (fun cases case ->
                  match case with
                  | 
                  { s = IfStmt (
                       cond,
                       case
          | CompoundStmt [] ->
              failwith "switch statement body must have at least one case"
          | _ -> failwith "switch statement body must be CompoundStmt"
        in
        *)

        (*
        let cases =
          List.fold_left (function
              | CaseStmt (expr, stmt) ->
                  IfStmt (
                    { 
        in
        *)

        let stmt = {
          s = CompoundStmt (
              Codegen.declare cond.e_sloc var
                (Types.tloc_of_ctyp cond.e_sloc cond.e_type);
              :: Codegen.assign var cond
              :: []
            );
          s_sloc = stmt.s_sloc;
          s_cref = Ref.null;
        } in

        (vars, stmt)

    | _ ->
        MapVisitor.visit_stmt v vars stmt

  in

  let v = MapVisitor.({ default with map_stmt }) in

  snd % MapVisitor.visit_decl v 0
