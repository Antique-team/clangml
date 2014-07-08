open Camlp4.PreCast
open OcamlTypes.Sig
open OcamlTypes.Process

module S = BatString

let codegen ocaml_types =
  let _loc = Loc.ghost in

  let visit_types = OcamlTypes.Type_graph.must_visit ocaml_types in

  let ocaml_types =
    List.filter
      (fun (name, ty) -> not (List.mem_assoc (name ^ "_") ocaml_types))
      ocaml_types
  in

  let ocaml_types =
    List.map
      (fun ((name, ty) as name_type) -> 
         if not (S.ends_with name "_") then
           name_type
         else
           let name = S.rchop name in
           match ty with
           | SumType st -> (name, SumType { st with st_name = name })
           | _ -> assert false;
      )
      ocaml_types
  in

  print_endline @@ Show_ocaml_types.show ocaml_types;

  <:str_item<
    open Ast
  >>
