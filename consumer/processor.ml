open Clang.Api
open Clang.Ast


let dump_ast ast =
  let open C_sig in
  let open Data_structures in
  print_endline "---- vars ----";
  StringMap.iter (fun name ob ->
      Format.printf "%s: %a\n"
        name Show_c_var.format ob
    ) ast.cp_vars;
  print_endline "---- funs ----";
  StringMap.iter (fun name ob ->
      Format.printf "%s: %a\n"
        name Show_c_fun.format ob
    ) ast.cp_funs;
  print_endline "---- types ----";
  StringMap.iter (fun name ob ->
      Format.printf "%s: %a\n"
        name Show_c_type.format ob
    ) ast.cp_types;
;;


let memcad_parse file =
  let fh = open_in file in

  begin try
    let lexbuf = Lexing.from_channel fh in
    let ast = C_parser.entry C_lexer.token lexbuf in
    C_utils.ppi_c_prog "" stdout ast;
    dump_ast ast;
  with
  | Parsing.Parse_error ->
      print_endline "!!!! MemCAD failed to parse file";
  | Failure "lexing: empty token" ->
      print_endline "!!!! MemCAD failed to tokenise file";
  | Assert_failure (file, line, column) ->
      print_endline @@ Printf.sprintf "!!!! MemCAD Assert_failure(\"%s\", %d, %d)"
        file line column;
  end;

  close_in fh;
;;


let process clang =
  let file, decl = request clang @@ Compose (Filename, TranslationUnit) in

  print_endline @@ "%% processing file " ^ file;
  print_endline "--------------------- MemCAD PP ---------------------";
  (*print_endline (Show.show<Clang.Ast.decl> decl);*)
  memcad_parse file;

  let _ = Analysis.All.analyse_decl clang decl in

  print_string "--------------------- Clang AST ---------------------";
  Format.printf "@[<v2>@,%a@]@."
    Clang.Pp.pp_decl decl;

  let decl = Transformation.All.transform_decl clang decl in
  print_string "--------------------- Simple AST --------------------";
  Format.printf "@[<v2>@,%a@]@."
    Clang.Pp.pp_decl decl;

  print_endline "----------------- Clang -> MemCAD -------------------";
  let ast = Transform.c_prog_from_decl clang decl in
  C_utils.ppi_c_prog "" stdout ast;
  dump_ast ast;
  print_endline "-----------------------------------------------------";
;;


let () =
  Printexc.record_backtrace true;
  try
    Clang.Api.parse (List.tl @@ Array.to_list Sys.argv) process;
  with Clang.Api.E error ->
    failwith @@ Clang.Api.string_of_error error
;;
