open Clang.Api
open Clang.Ast


let dump_vars ast =
  let open C_sig in
  let open C_show in
  let open Data_structures in
  Format.printf "---- <vars> ----@\n";
  StringMap.iter (fun name ob ->
      Format.printf "%s: %a@\n"
        name Show_c_var.format ob
    ) ast.cp_vars;
;;


let dump_funs ast =
  let open C_sig in
  let open C_show in
  let open Data_structures in
  Format.printf "---- <funs> ----@\n";
  StringMap.iter (fun name ob ->
      Format.printf "%s: %a@\n"
        name Show_c_fun.format ob
    ) ast.cp_funs;
;;


let dump_types ast =
  let open C_sig in
  let open C_show in
  let open Data_structures in
  Format.printf "---- <types> ----@\n";
  StringMap.iter (fun name ob ->
      Format.printf "%s: %a@\n"
        name Show_c_type.format ob
    ) ast.cp_types;
;;


let dump_ast ast =
  (*dump_vars ast;*)
  (*dump_funs ast;*)
  dump_types ast;
;;


let memcad_parse file =
  let fh = open_in file in

  let ast =
    try
      let lexbuf = Lexing.from_channel fh in
      let ast = C_parser.entry C_lexer.token lexbuf in
      C_utils.ppi_c_prog "" stdout ast;
      dump_ast ast;
      Some ast
    with
    | Parsing.Parse_error ->
        print_endline "!!!! MemCAD failed to parse file";
        None
    | Failure "lexing: empty token" ->
        print_endline "!!!! MemCAD failed to tokenise file";
        None
    | Assert_failure (file, line, column) ->
        print_endline @@ Printf.sprintf "!!!! MemCAD Assert_failure(\"%s\", %d, %d)"
          file line column;
        None
  in

  let ast =
    match ast with
    | None -> None
    | Some ast ->
        try
          let ast = C_process.process_c_prog ast in
          dump_ast ast;
          (*C_utils.ppi_c_prog "" stdout ast;*)
          Some ast
        with
        | Failure msg ->
            print_endline @@ "\n!!!! MemCAD failed to typecheck file:\n  " ^ msg;
            Some ast
  in

  close_in fh;

  ast


let process clang =
  let file, decl = request clang @@ Compose (Filename, TranslationUnit) in

  print_endline @@ "%% processing file " ^ file;
  print_endline "--------------------- MemCAD PP ---------------------";
  (*print_endline (Show.show<Clang.Ast.decl> decl);*)
  (*let memcad_ast = memcad_parse file in*)

  (*let () = Analysis.All.analyse_decl clang decl in*)

  print_string "--------------------- Clang CST ---------------------";
  Format.printf "@[<v2>@,%a@]@."
    Clang.Pp.pp_decl decl;

  let decl = Transformation.All.transform_decl clang decl in

  (* print_string "--------------------- Simple CST --------------------"; *)
  (* Format.printf "@[<v2>@,%a@]@." *)
  (*   Clang.Pp.pp_decl decl; *)

  print_string "--------------------- Clang AST ---------------------";
  Format.printf "@[<v2>@,%a@]@."
    Clang.Ast.Show_decl.format decl;

  (*print_endline "----------------- Clang -> MemCAD -------------------";*)
  (*let ast = Transform.c_prog_from_decl clang decl in*)
  (*C_utils.ppi_c_prog "" stdout ast;*)
  (*dump_ast ast;*)
  (*print_endline "-----------------------------------------------------";*)

  (*match memcad_ast with*)
  (*| None -> ()*)
  (*| Some memcad_ast ->*)
      (*if memcad_ast = ast then*)
        (*print_endline "MEMCAD AST = CLANG AST"*)
  ()
;;


let () =
  Printexc.record_backtrace true;
  try
    Clang.Api.parse (List.tl @@ Array.to_list Sys.argv) process;
  with Clang.Api.E error ->
    failwith @@ Clang.Api.string_of_error error
;;
