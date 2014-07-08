open Camlp4.PreCast
open Sig


let _loc = Loc.ghost


let str_item_of_ocaml_type = function
  | AliasType (loc, name, ty) ->
      <:str_item<type aliastype = something>>
  | SumType sum_type ->
      <:str_item<type sumtype = A | B>>
  | RecordType record_type ->
      <:str_item<type recordtype = { a : int }>>
  | RecursiveType (loc, types) ->
      <:str_item<type rec1 = C | D of rec2 and rec2 = E | F of rec1>>
  | Version (loc, version) ->
      <:str_item<let version = $str:version$>>


let codegen types =
  print_endline @@ Show_ocaml_type.show_list types;

  let items =
    List.map str_item_of_ocaml_type types
  in

  List.fold_left
    (fun code item ->
      <:str_item<
        $code$;; $item$;;
      >>
    )
    <:str_item<
      open Ast
    >>
    items
