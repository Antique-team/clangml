open Camlp4.PreCast
open Sig

module L = BatList


let _loc = Loc.ghost

let ctyp_of_basic_type bt =
  assert false


let ctyp_of_sum_type_branches branches =
  List.map
    (fun b ->
       match b.stb_types with
       | [] -> <:ctyp<$uid:b.stb_name$>>
       | bt1 :: bts ->
           let bt = ctyp_of_basic_type bt1 in
           <:ctyp<$uid:b.stb_name$ of bt >>
    )
    branches
  |>
  L.reduce
    (fun acc ty -> <:ctyp< $acc$ | $ty$ >>)

let str_item_of_ocaml_type = function
  | AliasType (loc, name, ty) -> assert false;
  | SumType st ->
   (*
    type language =
    | Lang_C
    | Lang_CXX
   *)
      let branches = ctyp_of_sum_type_branches st.st_branches in
      <:str_item<type $lid:st.st_name$ = | $branches$ >>
  | RecordType rt ->
      <:str_item<type $lid:rt.rt_name$ = { a : int }>>
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
