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


let check_name name kind clang sloc state =
  if Api.(request clang @@ IsFromMainFile sloc.Ast.loc_s) then
    let state =
      if String.length name >= 2 then
        if name.[0] = '_' && name.[1] = '_' then
          (sloc, "reserved " ^ kind ^ " name `" ^ name ^ "'") :: state
        else if name.[0] = '_' && is_uppercase name.[1] then
          (sloc, "reserved " ^ kind ^ " name `" ^ name ^ "'") :: state
        else
          state
      else
        state
    in

    state

  else
    state


let analyse_decl clang decl =
  let open Ast in

  let rec fold_decl v state decl =
    let check =
      match decl.d with
      | VarDecl (_, name, _) ->
          check_name name "variable"
      | FunctionDecl (_, name, _) ->
          check_name name "function"
      | TypedefDecl (_, name) ->
          check_name name "typedef"
      | RecordDecl (name, _) ->
          check_name name "record"

      | _ ->
          fun _ _ state -> state
    in

    let state = check clang decl.d_sloc state in

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
      failwith @@ "naming convention check failed with " ^ string_of_int
                    (List.length warnings) ^ " violations"
