open Camlp4.PreCast
open OcamlTypes.Sig
open OcamlTypes.Print
open OcamlTypes.Process


let parse_and_generate dir source =
  let ocaml_types =
    OcamlTypes.Parse.parse_file source
    |> flatten_and_map
  in

  let visit_types = OcamlTypes.Type_graph.must_visit ocaml_types in

  List.iter (fun kind ->
    let name = GenerateVisitor.name_of_kind kind in
    let output_file = dir ^ "/" ^ name ^ "Visitor.ml" in
    let impl = GenerateVisitor.codegen kind visit_types ocaml_types in
    OCamlPrinter.print_implem ~output_file impl;
    (*OCamlDumper.print_implem ~output_file impl;*)
  ) GenerateVisitor.([Map; Fold; Iter])


let () =
  Printexc.record_backtrace true;
  match Sys.argv with
  | [|_; dir; source|] ->
      parse_and_generate dir source
  | _ ->
      print_endline "Usage: bridgen <output-path> <source>"
