open Camlp4.PreCast
open OcamlTypes.Sig
open OcamlTypes.Process


(* TODO: two new function shapes:

(* Record_type *)

let map_field_decl v state field_decl =
  (* map *)
  let (state, fd_type) = v.map_tloc v state field_decl.fd_type in
  (* fold *)
  let state = fold_option v.fold_tloc v state field_decl.fd_bitw in
  (* iter *)
  let () = iter_option v.iter_tloc v field_decl.fd_init in
  (state, {
      field_decl with
      fd_type;
      fd_bitw;
      fd_init;
  })

(* Combined_type *)

let visit_desg v state desg =
  let (state, dr) =
    match desg.dr with
    | FieldDesignator string0 -> (state, (FieldDesignator string0))
    | ArrayDesignator expr0 ->
        let (state, expr0) = v.map_expr v state expr0
        in (state, (ArrayDesignator expr0))
    | ArrayRangeDesignator (expr0, expr1) ->
        let (state, expr0) = v.map_expr v state expr0 in
        let (state, expr1) = v.map_expr v state expr1
        in (state, (ArrayRangeDesignator (expr0, expr1)))
  in (state, { (desg) with dr = dr; });;

(* Sum_type *)

let map_offsetof_node v state = function
  | OON_Array expr0 ->
      let (state, expr0) = v.visit_expr v state expr0 in
      (state, OON_Array expr0)
  | OON_Field name -> (state, OON_Field name)
  | OON_Identifier name -> (state, OON_Identifier name)
  | OON_Base cxx_base_specifier0 -> (state, OON_Base cxx_base_specifier0)
*)

type type_kind =
  | Record_type of record_type
  | Combined_type of record_type * sum_type
  | Sum_type of sum_type


let classify_type ocaml_types name =
  (* a combined type is a record type with an associated sum type
     named the same but with a trailing _ *)
  try
    let s_type = match List.assoc (name ^ "_") ocaml_types with
      | SumType st -> st
      | _ -> assert false
    in
    let r_type = match List.assoc name ocaml_types with
      | RecordType rt -> rt
      | _ -> assert false
    in
    Combined_type (r_type, s_type)

  with Not_found ->

    match List.assoc name ocaml_types with
    (* a record type is any record that is not a combined type *)
    | RecordType rt -> Record_type rt
    (* a sum type is given by the remaining cases *)
    | SumType st -> Sum_type st
    | _ -> assert false


type kind =
  | Map
  | Fold
  | Iter

let name_of_kind = function
  | Map -> "map"
  | Fold -> "fold"
  | Iter -> "iter"

let kind_has_state = function
  | Map | Fold -> true
  | Iter -> false


let reduce f = function
  | [] -> assert false
  | fn :: fns -> List.fold_left f fn fns


let loc_of_type ocaml_types name =
  match List.assoc name ocaml_types with
  | RecordType { rt_loc = loc }
  | SumType { st_loc = loc } ->
      loc
  | _ -> assert false


(****************************************************************************
 * Single match case
 ****************************************************************************)


let make_update kind visit_type_names sum_ty result =
  let prefix = name_of_kind kind ^ "_" in

  let _loc = sum_ty.stb_loc in
  let tycon = sum_ty.stb_name in
  let tycon_args = sum_ty.stb_types in

  (* Update calls: call visitor on everything that may need to be updated. *)
  List.mapi (fun i ty -> (i, ty)) tycon_args
  |> List.rev
  |> List.fold_left (fun expr (i, ty) ->
       let mangle name =
         name ^ string_of_int i
       in

       (* Map a value, optionally with an extra mapping function
          (e.g. map_list or map_option). The result is the updated value. *)
       let mkmap name = function
         | None ->
             let var = mangle name in
             if kind_has_state kind then
               <:expr<v.$lid:prefix ^ name$ v state $lid:var$>>
             else
               <:expr<v.$lid:prefix ^ name$ v $lid:var$>>
         | Some fn ->
             let var = mangle name in
             if kind_has_state kind then
               <:expr<$lid:prefix ^ fn$ v.$lid:prefix ^ name$ v state $lid:var$>>
             else
               <:expr<$lid:prefix ^ fn$ v.$lid:prefix ^ name$ v $lid:var$>>
       in

       let update =
         match ty with
         | ListOfType (_, NamedType (_loc, name))
           when List.mem name visit_type_names ->
             Some (name, mkmap name (Some "list"))
         | OptionType (_, NamedType (_loc, name))
           when List.mem name visit_type_names ->
             Some (name, mkmap name (Some "option"))
         | NamedType (_loc, name)
           when List.mem name visit_type_names ->
             Some (name, mkmap name None)
         | _ ->
             None
       in

       match update with
       | None -> expr
       | Some (name, update) ->
           let name = mangle name in
           let patt =
             match kind with
             | Map -> <:patt<(state, $lid:name$)>>
             | Fold -> <:patt<state>>
             | Iter -> <:patt<()>>
           in
           <:expr<
             let $patt$ = $update$ in
             $expr$
           >>
     ) result


let make_match_case kind visit_type_names sum_ty =
  let _loc = sum_ty.stb_loc in
  let tycon = sum_ty.stb_name in
  let tycon_args = sum_ty.stb_types in

  (* Generic construction function for patterns and expressions. *)
  let construct init mkty reduce =
    List.mapi (fun i -> function
      | ListOfType (_, NamedType (_loc, name))
      | OptionType (_, NamedType (_loc, name))
      | OptionType (_, ListOfType (_, NamedType (_loc, name)))
      | ClangType (_loc, name)
      | NamedType (_loc, name) ->
          _loc, mkty i _loc name
      | _ ->
          failwith @@ "unsupported argument type in tycon " ^ tycon
    ) tycon_args
    |> List.fold_left (fun tycon (_loc, param) ->
         reduce _loc tycon param
       ) init
  in

  (* Matching pattern. *)
  let pattern =
    construct <:patt<$uid:tycon$>>
      (fun i _loc name ->
         <:patt<$lid:name ^ string_of_int i$>>)
      (fun _loc tycon param ->
         Ast.PaApp (_loc, tycon, param))
  in

  (* Reconstruction expression. *)
  let result =
    match kind with
    | Map ->
        let reconstruct =
          construct <:expr<$uid:tycon$>>
            (fun i _loc name ->
               <:expr<$lid:name ^ string_of_int i$>>)
            (fun _loc tycon param ->
               Ast.ExApp (_loc, tycon, param))
        in
        <:expr<(state, $reconstruct$)>>
    | Fold ->
        <:expr<state>>
    | Iter ->
        <:expr<()>>
  in

  let update = make_update kind visit_type_names sum_ty result in

  <:match_case<$pattern$ -> $update$>>


(****************************************************************************
 * Functions
 ****************************************************************************)

let make_combined_type_function kind visit_types name rec_ty sum_ty =
  let rec_loc = rec_ty.rt_loc in

  let match_cases =
    List.map
      (make_match_case kind visit_types)
      sum_ty.st_branches
    |> reduce (fun cases case ->
         let _loc = Ast.loc_of_match_case case in
         <:match_case<$cases$ | $case$>>
       )
  in

  let { rtm_name = main_member } =
    List.find (function
      | { rtm_type = NamedType (_, member) } -> member = name ^ "_"
      | _ -> false
    ) rec_ty.rt_members
  in

  let do_match =
    <:expr@rec_loc<
      match $lid:name$.$lid:main_member$ with
      $match_cases$
    >>
  in

  let body =
    match kind with
    | Map ->
        <:expr@rec_loc<
          let (state, $lid:main_member$) =
            $do_match$
          in
          (state, { $lid:name$ with $lid:main_member$ })
        >>
    | Fold | Iter ->
        do_match
  in

  if kind_has_state kind then
    <:str_item@rec_loc<
      let $lid:"visit_" ^ name$ v state $lid:name$ =
        $body$
    >>
  else
    <:str_item@rec_loc<
      let $lid:"visit_" ^ name$ v $lid:name$ =
        $body$
    >>


let make_sum_type_function kind visit_types name st =
  let _loc = st.st_loc in

  let prefix = name_of_kind kind ^ "_" in

  let match_cases =
    List.map
      (make_match_case kind visit_types)
      st.st_branches
    |> reduce (fun cases case ->
         let _loc = Ast.loc_of_match_case case in
         <:match_case<$cases$ | $case$>>
       )
  in

  if kind_has_state kind then
    <:str_item<
      let $lid:"visit_" ^ name$ v state = function
        $match_cases$
    >>
  else
    <:str_item<
      let $lid:"visit_" ^ name$ v = function
        $match_cases$
    >>


let make_record_type_function kind visit_types name rt =
  let _loc = rt.rt_loc in

  let visit_name = "visit_" ^ name in
  match kind with
  | Map ->
      <:str_item<
        let $lid:visit_name$ v state $lid:name$ =
          (state, $lid:name$)
      >>
  | Fold ->
      <:str_item<
        let $lid:visit_name$ v state $lid:name$ =
          state
      >>
  | Iter ->
      <:str_item<
        let $lid:visit_name$ v $lid:name$ =
          ()
      >>


let make_functions kind visit_types ocaml_types =
  let prefix = name_of_kind kind ^ "_" in

  List.map
    (fun name ->
       match classify_type ocaml_types name with
       | Record_type rt ->
           make_record_type_function   kind visit_types name rt
       | Combined_type (rt, st) ->
           make_combined_type_function kind visit_types name rt st
       | Sum_type st -> 
           make_sum_type_function      kind visit_types name st
    )
    visit_types
  |> reduce (fun functions fn ->
       let _loc = Ast.loc_of_str_item fn in
       <:str_item<$functions$;; $fn$>>
     )


(****************************************************************************
 * Members
 ****************************************************************************)


let make_members kind ocaml_types visit_types =
  let prefix = name_of_kind kind ^ "_" in

  let mkty name =
    let _loc = loc_of_type ocaml_types name in

    let result_ty =
      match kind with
      | Map -> <:ctyp<'a * $lid:name$>>
      | Fold -> <:ctyp<'a>>
      | Iter -> <:ctyp<unit>>
    in

    let visit_ty =
      match kind with
      | Map | Fold ->
          <:ctyp<'a visitor -> 'a -> $lid:name$ -> $result_ty$>>
      | Iter ->
          <:ctyp<visitor -> $lid:name$ -> $result_ty$>>
    in

    <:ctyp<
      $id:(<:ident< $lid:prefix ^ name$ >>)$
        : $visit_ty$
    >>
  in

  List.map mkty visit_types
  |> reduce (fun members ty ->
       let _loc = Ast.loc_of_ctyp ty in
       <:ctyp<$members$; $ty$>>
     )


(****************************************************************************
 * Default
 ****************************************************************************)


let make_default kind visit_types ocaml_types =
  let prefix = name_of_kind kind ^ "_" in

  let mkbinding name =
    let _loc = loc_of_type ocaml_types name in
    <:rec_binding<$lid:prefix ^ name$ = $lid:"visit_" ^ name$>>
  in

  List.map mkbinding visit_types
  |> reduce (fun members ty ->
       let _loc = Ast.loc_of_rec_binding ty in
       <:rec_binding<$members$; $ty$>>
     )


(****************************************************************************
 * Codegen entry point
 ****************************************************************************)


let codegen kind (visit_types : string list) ocaml_types =
  let visit_types =
    List.filter
      (fun name -> name.[String.length name - 1] <> '_')
      visit_types
  in

  let members = make_members kind ocaml_types visit_types in

  let _loc = Loc.ghost in

  let tydcl =
    match kind with
    | Map | Fold ->
        <:str_item<type 'a visitor = { $members$; }>>
    | Iter ->
        <:str_item<type    visitor = { $members$; }>>
  in

  let functions = make_functions kind visit_types ocaml_types in
  let default = make_default kind visit_types ocaml_types in

  (* Put it all together. *)
  <:str_item<
    open Ast
    open Visitor

    $tydcl$;;

    $functions$;;
    let default = { $default$ };;
  >>
