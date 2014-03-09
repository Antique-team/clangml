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

  Unix.mkdir dir 0o777;

  [
    "map", GenerateMapVisitor. codegen ocaml_types;
    "fold", GenerateFoldVisitor.codegen ocaml_types;
  ]
  |> List.iter (fun (name, impl) ->
      let output_file = dir ^ "/" ^ name ^ "Visitor.ml" in
      (*OCamlPrinter.print_implem ~output_file impl;*)
      OCamlDumper.print_implem ~output_file impl;
    )


let () =
  Printexc.record_backtrace true;
  match Sys.argv with
  | [|_; dir; source|] ->
      parse_and_generate dir source
  | _ ->
      print_endline "Usage: bridgen <output-path> <source>"
