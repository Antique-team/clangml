open Camlp4.PreCast
open Sig


let codegen types =
  let _loc = Loc.ghost in

  print_endline @@ Show_ocaml_type.show_list types;

  <:str_item<
    open Ast
  >>
