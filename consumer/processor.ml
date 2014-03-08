open Clang.Api
open Clang.Ast


let memcad_parse file =
  let fh = open_in file in

  begin try
    let lexbuf = Lexing.from_channel fh in
    let ast = C_parser.entry C_lexer.token lexbuf in
    C_utils.ppi_c_prog "" stderr ast;
  with
  | Parsing.Parse_error ->
      prerr_endline "!!!! MemCAD failed to parse file";
  | Failure "lexing: empty token" ->
      prerr_endline "!!!! MemCAD failed to tokenise file";
  | Assert_failure (file, line, column) ->
      prerr_endline @@ Printf.sprintf "!!!! MemCAD Assert_failure(\"%s\", %d, %d)"
        file line column;
  end;

  close_in fh;
;;


let process clang =
  let file, decl = request clang @@ Compose (Filename, TranslationUnit) in

  prerr_endline @@ "%% processing file " ^ file;
  prerr_endline "--------------------- MemCAD PP ---------------------";
  (*prerr_endline (Show.show<Clang.Ast.decl> decl);*)
  memcad_parse file;

  prerr_string "--------------------- Clang AST ---------------------";
  Format.fprintf Format.err_formatter "@[<v2>@,%a@]@."
    Clang.Pp.pp_decl decl;

  let decl = Transforms.All.transform_decl clang decl in
  prerr_string "--------------------- Simple AST --------------------";
  Format.fprintf Format.err_formatter "@[<v2>@,%a@]@."
    Clang.Pp.pp_decl decl;

  prerr_endline "----------------- Clang -> MemCAD -------------------";
  C_utils.ppi_c_prog "" stderr (Transform.c_prog_from_decl decl);
  prerr_endline "-----------------------------------------------------";
;;


let () =
  Printexc.record_backtrace true;
  Clang.Api.connect process;
;;
