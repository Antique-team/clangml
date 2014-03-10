open Clang.Api
open Clang.Ast


let memcad_parse file =
  let fh = open_in file in

  begin try
    let lexbuf = Lexing.from_channel fh in
    let ast = C_parser.entry C_lexer.token lexbuf in
    C_utils.ppi_c_prog "" stdout ast;
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

  print_string "--------------------- Clang AST ---------------------";
  Format.printf "@[<v2>@,%a@]@."
    Clang.Pp.pp_decl decl;

  let decl = Transforms.All.transform_decl clang decl in
  print_string "--------------------- Simple AST --------------------";
  Format.printf "@[<v2>@,%a@]@."
    Clang.Pp.pp_decl decl;

  print_endline "----------------- Clang -> MemCAD -------------------";
  C_utils.ppi_c_prog "" stdout (Transform.c_prog_from_decl decl);
  print_endline "-----------------------------------------------------";
;;


let () =
  Printexc.record_backtrace true;
  Clang.Api.parse (List.tl @@ Array.to_list Sys.argv) process;
;;
