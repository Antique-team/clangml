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

let rec ctyp_of_ocaml_type = function
  | SumType st ->
   (*
    type language =
    | Lang_C
    | Lang_CXX
   *)
      let branches = ctyp_of_sum_type_branches st.st_branches in
      <:ctyp<$lid:st.st_name$ = | $branches$ >>

  | RecordType rt ->
      <:ctyp<$lid:rt.rt_name$ = { a : int }>>

  | RecursiveType (loc, types) ->
      let ctyps = List.map ctyp_of_ocaml_type types in
      BatList.reduce (fun acc ty ->
          <:ctyp<$acc$ and $ty$>>
        ) ctyps

  | AliasType (loc, name, ty) ->
      assert false
  | Version (loc, version) ->
      assert false


let codegen types =
  print_endline @@ Show_ocaml_type.show_list types;

  let items =
    types
    |> List.filter 
         (function
           | Version _ -> false
           | _ -> true
         )
    |> List.map ctyp_of_ocaml_type
    |> List.map (fun ctyp -> <:str_item<type $ctyp$>>)
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
