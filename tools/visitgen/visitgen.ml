open Camlp4.PreCast
open ProcessOCamlTypes


let code_gen dir (ocaml_types : Parse.ocaml_type list) =
  let ctx = make_context ocaml_types in

  let visit_types =
    List.filter (fun ty ->
      List.mem (ty ^ "_") ctx.class_types
    ) ctx.class_types
    |> List.sort String.compare
  in
  List.iter print_endline visit_types;

  let _loc = Loc.ghost in

  let impl =
    <:str_item<
      open Ast
    >>
  in

  ()
;;


let parse_and_generate dir source =
  let ocaml_types = Parse.parse_file source in
  print_endline (Show.show_list<Parse.ocaml_type> ocaml_types);
  code_gen dir ocaml_types


let () =
  match Sys.argv with
  | [|_; dir; source|] ->
      parse_and_generate dir source
  | _ ->
      print_endline "Usage: bridgen <output-path> <source>"
