open Camlp4.PreCast
open DefineOCamlTypes
open PrintOCamlTypes
open ProcessOCamlTypes


let parse_and_generate dir source =
  let ocaml_types =
    Parse.parse_file source
    |> flatten_recursive_types
    |> List.map (function
        | RecordType (_, name, _)
        | SumType (_, name, _) as ty ->
            (name, ty)
        | _ -> failwith "invalid type returned from flatten_recursive_types"
      )
  in

  List.iter (fun kind ->
    let name = GenerateVisitor.name_of_kind kind in
    let output_file = dir ^ "/" ^ name ^ "Visitor.ml" in
    let impl = GenerateVisitor.codegen kind ocaml_types in
    (*OCamlPrinter.print_implem impl;*)
    OCamlDumper.print_implem ~output_file impl;
  ) GenerateVisitor.([Map; Fold])


let () =
  Printexc.record_backtrace true;
  match Sys.argv with
  | [|_; dir; source|] ->
      parse_and_generate dir source
  | _ ->
      print_endline "Usage: bridgen <output-path> <source>"
