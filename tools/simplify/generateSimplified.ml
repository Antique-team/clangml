open Camlp4.PreCast
open OcamlTypes.Sig
open OcamlTypes.Process


let codegen ocaml_types =
  let _loc = Loc.ghost in

  let type_map = make_type_map ocaml_types in

  let simplified : sum_type list =
    List.map (fun (name, _, { st_loc = loc; st_branches = branches }) ->
        {
          st_loc = loc;
          st_name = name;
          st_branches = branches;
        }
    ) type_map
  in

  (*print_endline @@ Show_type_map.show_list type_map;*)

  <:str_item<
    open Ast
  >>
