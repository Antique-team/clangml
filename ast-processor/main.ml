open ClangAst


let memcad_parse file =
  let fh = open_in file in
  let lexbuf = Lexing.from_channel fh in
  let ast = C_parser.entry C_lexer.token lexbuf in
  close_in fh;
  C_utils.ppi_c_prog "" stdout ast;
;;


let () =
  let open ClangApi in

  match ClangApi.recv () with
  | List [Filename file; AstNode (Decl decl)] ->
      memcad_parse file;
      Format.printf "@[<v2>Declaration:@,%a@]@."
        ClangPp.pp_decl decl;
      C_utils.ppi_c_prog "" stdout (Transform.c_prog_from_decl decl)

  | _ ->
      failwith "Unhandled message type"
