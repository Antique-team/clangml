open Camlp4.PreCast
open OcamlTypes.Sig


let _loc = Loc.ghost


let codegen ocaml_types filtered_types =
  <:str_item<
    let simplify = function
      | _ -> assert false
  >>
