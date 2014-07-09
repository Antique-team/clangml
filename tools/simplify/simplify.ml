open Camlp4.PreCast
open OcamlTypes.Sig
open OcamlTypes.Print
open OcamlTypes.Process


let parse_and_generate dir srcmod =
  let source = dir ^ "/" ^ srcmod ^ "Bridge.ml" in
  let ocaml_types = OcamlTypes.Parse.parse_file source in

  let (types, filtered_types) =
    GenerateSimplified.codegen
      ocaml_types
  in

  let simplify =
    ToSimple.codegen
      (String.capitalize srcmod)
      ocaml_types
      filtered_types
  in

  let types_file    = dir ^ "/" ^ srcmod ^ "Simple.ml" in
  let simplify_file = dir ^ "/" ^ srcmod ^ "Simplify.ml" in

  OCamlPrinter.print_implem ~output_file:types_file    types;
  OCamlPrinter.print_implem ~output_file:simplify_file simplify;
  (*OCamlDumper.print_implem ~output_file impl;*)
;;


let () =
  Printexc.record_backtrace true;
  match Sys.argv with
  | [|_; dir; srcmod|] ->
      parse_and_generate dir srcmod
  | _ ->
      print_endline "Usage: simplify <output-path> <source>"
