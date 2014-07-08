open Camlp4.PreCast
open OcamlTypes.Sig
open OcamlTypes.Print
open OcamlTypes.Process


let parse_and_generate output_file source =
  let ocaml_types = OcamlTypes.Parse.parse_file source in

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
