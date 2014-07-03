open Camlp4.PreCast
open OcamlTypes.Sig
open OcamlTypes.Process


let codegen ocaml_types =
  let _loc = Loc.ghost in

  let visit_types = visit_types ocaml_types in

  let simplified : sum_type list =
    List.map (fun (name, _, (loc, _, branches)) ->
      (loc, name, branches)
    ) visit_types
  in

  print_endline @@ Show_type_map.show_list visit_types;

  <:str_item<
    open Ast
  >>
