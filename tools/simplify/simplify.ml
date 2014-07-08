open Camlp4.PreCast
open OcamlTypes.Sig
open OcamlTypes.Print
open OcamlTypes.Process


let parse_and_generate output_file source =
  let ocaml_types =
    OcamlTypes.Parse.parse_file source
    |> flatten_recursive_types
    |> List.map (function
        | RecordType { rt_name = name }
        | SumType { st_name = name } as ty ->
            (name, ty)
        | _ -> failwith "invalid type returned from flatten_recursive_types"
      )
  in

  let impl = GenerateSimplified.codegen ocaml_types in
  OCamlPrinter.print_implem ~output_file impl;
  (*OCamlDumper.print_implem ~output_file impl;*)
;;


let () =
  Printexc.record_backtrace true;
  match Sys.argv with
  | [|_; output_file; source|] ->
      parse_and_generate output_file source
  | _ ->
      print_endline "Usage: simplify <output-path> <source>"
