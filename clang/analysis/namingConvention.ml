open Clang
open Prelude
open Diagnostic


let is_uppercase ch =
  ch = Char.uppercase ch


let warn sloc kind name =
  Diagnostic.({
    diag_loc = sloc.Ast.loc_s;
    diag_msg = "reserved " ^ kind ^ " name `" ^ name ^ "'";
    diag_std = Std_C "7.1.3";
    diag_lvl = Warning;
    diag_flg = Wreserved_name;
  })


let check_name name kind clang sloc state =
  if Sloc.is_valid sloc.Ast.loc_s &&
     Api.(request clang @@ FileCharacteristic sloc.Ast.loc_s) = Sloc.C_User then
    let state =
      if String.length name >= 2 then
        if name.[0] = '_' && name.[1] = '_' then
          warn sloc kind name :: state
        else if name.[0] = '_' && is_uppercase name.[1] then
          warn sloc kind name :: state
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
      | FunctionDecl (_, DN_Identifier name, _) ->
          check_name name "function"
      | TypedefDecl (_, name) ->
          check_name name "typedef"
      | RecordDecl (_, name, _, _) ->
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
      List.iter (Diagnostic.show clang) warnings;
      failwith @@ "naming convention check failed with " ^ string_of_int
                    (List.length warnings) ^ " violations"
