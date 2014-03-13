open Clang
open Prelude


let is_uppercase ch =
  ch = Char.uppercase ch


let print_warning clang (loc, w) =
  let ploc = Api.(request clang @@ PresumedLoc loc.Ast.loc_s) in
  Printf.printf "%s:%d: %s\n"
    ploc.Sloc.loc_filename
    ploc.Sloc.loc_line
    w


let check_name clang sloc name state =
  if Api.(request clang @@ IsFromMainFile sloc.Ast.loc_s) then
    let state =
      if name.[0] = '_' && name.[1] = '_' then
        (sloc, "reserved name `" ^ name ^ "'") :: state
      else if name.[0] = '_' && is_uppercase name.[1] then
        (sloc, "reserved name `" ^ name ^ "'") :: state
      else
        state
    in

    state

  else
    state


let analyse_decl clang decl =
  let open Ast in

  let rec fold_decl v state decl =
    match decl.d with
    | VarDecl (_, name, _)
    | FunctionDecl (_, name, _)
    | TypedefDecl (_, name) ->
        let state = FoldVisitor.visit_decl v state decl in

        check_name clang decl.d_sloc name state

    | _ ->
        FoldVisitor.visit_decl v state decl

  in

  let v = FoldVisitor.({ default with fold_decl }) in

  let warnings =
    FoldVisitor.visit_decl v [] decl
    |> List.rev
  in

  match warnings with
  | [] -> () (* OK *)
  | warnings ->
      List.iter (print_warning clang) warnings;
      failwith "naming convention check failed"
