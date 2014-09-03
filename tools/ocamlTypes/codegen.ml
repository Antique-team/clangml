open Camlp4.PreCast
open Sig

module L = BatList


let rec ctyp_of_basic_type = function
  (* <:ctyp<$bt$>> *)
  | NamedType  (_loc, id)  -> <:ctyp<$lid:id$>>
  | SourceLocation _loc    -> <:ctyp<Sloc.t>>
  | ClangType  (_loc, str) -> assert false;
  | RefType    (_loc, bt)  -> assert false;
  | ListOfType (_loc, bt)  ->
      let ty = ctyp_of_basic_type bt in
      <:ctyp<$ty$ list>>
  | OptionType (_loc, bt)  ->
      let ty = ctyp_of_basic_type bt in
      <:ctyp<$ty$ option>>
  | TupleType  (_loc, bts) ->
      let tys =
        List.map ctyp_of_basic_type bts
        |> BatList.reduce
             (fun members ty ->
                Ast.TySta (_loc, members, ty))
      in
      Ast.TyTup (_loc, tys)

let ctyp_of_sum_type_branches _loc branches =
  List.map
    (fun b ->
       match b.stb_types with
       | [] -> <:ctyp<$uid:b.stb_name$>>
       | bts ->
           let members =
             List.map ctyp_of_basic_type bts
             |> BatList.reduce
                  (fun members ty ->
                     Ast.TyAnd (_loc, members, ty))
           in
           <:ctyp<$uid:b.stb_name$ of $members$>>
    )
    branches
  |> L.reduce (fun acc ty -> <:ctyp<$acc$ | $ty$>>)

let ctyp_of_record_type_members _loc members =
  List.map
    (fun m ->
       let ctyp = ctyp_of_basic_type m.rtm_type in
       <:ctyp<$uid:m.rtm_name$ : $ctyp$>>
    )
    members
  |> L.reduce (fun acc ty -> <:ctyp<$acc$ ; $ty$>>)

let rec ctyp_of_ocaml_type = function
  | SumType st ->
   (*
    type language =
    | Lang_C
    | Lang_CXX
   *)
      let _loc = st.st_loc in
      let branches = ctyp_of_sum_type_branches _loc st.st_branches in
      <:ctyp<$lid:st.st_name$ = | $branches$>>

  | RecordType rt ->
      let _loc = rt.rt_loc in
      let members = ctyp_of_record_type_members _loc rt.rt_members in
      <:ctyp<$lid:rt.rt_name$ = { $members$ }>>

  | RecursiveType (_loc, types) ->
      let ctyps = List.map ctyp_of_ocaml_type types in
      BatList.reduce (fun acc ty ->
          <:ctyp<$acc$ and $ty$>>
        ) ctyps

  | AliasType (loc, name, ty) ->
      assert false
  | Version (loc, version) ->
      assert false


let codegen types =
  let _loc = Loc.ghost in

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

  BatList.reduce
    (fun code item ->
      <:str_item<
        $code$;; $item$;;
      >>
    )
    items
