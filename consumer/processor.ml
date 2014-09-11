open Clang.Api
open Clang.Ast

let process clang =
  let file, decl = request clang @@ Compose (Filename, TranslationUnit) in
  print_endline @@ "%% processing file " ^ file;
  print_string "--------------------- Clang CST ---------------------";
  Format.printf "@[<v2>@,%a@]@."
    Clang.Pp.pp_decl decl;
  Printf.printf "--------------------- gory details ---------------------\n";
  Printf.printf "%s\n" (Clang.Pp.string_of_decl_ decl.d)

let () =
  Printexc.record_backtrace true;
  try
    Clang.Api.parse (List.tl @@ Array.to_list Sys.argv) process;
  with Clang.Api.E error ->
    failwith @@ Clang.Api.string_of_error error
;;
