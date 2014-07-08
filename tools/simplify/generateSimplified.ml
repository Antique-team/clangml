open Camlp4.PreCast
open OcamlTypes.Sig
open OcamlTypes.Process


let codegen ocaml_types =
  let _loc = Loc.ghost in

  let visit_types = OcamlTypes.Type_graph.must_visit ocaml_types in

  print_endline @@ Show_ocaml_types.show ocaml_types;

  <:str_item<
    open Ast
  >>
