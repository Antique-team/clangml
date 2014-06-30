open Camlp4.PreCast
open OcamlTypes.Define
open OcamlTypes.Process


type kind =
  | Map
  | Fold
  | Iter

let name_of_kind = function
  | Map -> "map"
  | Fold -> "fold"
  | Iter -> "iter"


let reduce f = function
  | [] -> assert false
  | fn :: fns -> List.fold_left f fn fns


let codegen kind ocaml_types =
  let ctx = make_context @@ snd @@ List.split ocaml_types in

  let prefix = name_of_kind kind ^ "_" in

  let visit_types =
    List.filter (fun (name, _) ->
      List.mem (name ^ "_") ctx.class_types
    ) ocaml_types
    |> List.map (fun (name, rec_ty) ->
        let rec_ty =
          match rec_ty with
          | RecordType ty -> ty
          | _ -> failwith @@ "type " ^ name ^ "_ is not a record type"
        in
        let sum_ty =
          match List.assoc (name ^ "_") ocaml_types with
          | SumType ty -> ty
          | _ -> failwith @@ "type " ^ name ^ " is not a sum type"
        in
        (name, rec_ty, sum_ty)
      )
  in

  let visit_type_names =
    List.map
      (fun (name, _, _) -> name)
      visit_types
  in


  let functions =
    List.map (fun (name, rec_ty, sum_ty) ->
      let rec_loc, rec_name, rec_mems = rec_ty in
      let sum_loc, sum_name, sum_mems = sum_ty in

      let match_cases =
        List.map (fun (_loc, tycon, tycon_args) ->
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

          let pattern =
            construct <:patt<$uid:tycon$>>
              (fun i _loc name -> 
                 <:patt<$lid:name ^ string_of_int i$>>)
              (fun _loc tycon param ->
                 Ast.PaApp (_loc, tycon, param))
          in

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

          let update =
            List.mapi (fun i ty -> (i, ty)) tycon_args
            |> List.rev
            |> List.fold_left (fun expr (i, ty) ->
                let mangle name =
                  name ^ string_of_int i
                in

                let mkmap name = function
                  | None ->
                      let var = mangle name in
                      begin match kind with
                      | Map | Fold ->
                          <:expr<v.$lid:prefix ^ name$ v state $lid:var$>>
                      | Iter ->
                          <:expr<v.$lid:prefix ^ name$ v $lid:var$>>
                      end
                  | Some fn ->
                      let var = mangle name in
                      begin match kind with
                      | Map | Fold ->
                          <:expr<$lid:prefix ^ fn$ v.$lid:prefix ^ name$ v state $lid:var$>>
                      | Iter ->
                          <:expr<$lid:prefix ^ fn$ v.$lid:prefix ^ name$ v $lid:var$>>
                      end
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
          in

          <:match_case<$pattern$ -> $update$>>
        ) sum_mems

        |> reduce (fun cases case ->
            let _loc = Ast.loc_of_match_case case in
            <:match_case<$cases$ | $case$>>
          )
      in

      let (_, main_member, _) =
        List.find (function
          | (_, _, NamedType (_, member)) -> member = name ^ "_"
          | _ -> false
        ) rec_mems
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

      match kind with
      | Map | Fold ->
          <:str_item@rec_loc<
            let $lid:"visit_" ^ name$ v state $lid:name$ =
              $body$
          >>
      | Iter ->
          <:str_item@rec_loc<
            let $lid:"visit_" ^ name$ v $lid:name$ =
              $body$
          >>
    ) visit_types
    |> reduce (fun functions fn ->
        let _loc = Ast.loc_of_str_item fn in
        <:str_item<$functions$;; $fn$>>
      )
  in


  let members =
    let mkty (name, (_loc, _, _), _) =
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
  in


  let default =
    let mkbinding (name, (_loc, _, _), _) =
      <:rec_binding<$lid:prefix ^ name$ = $lid:"visit_" ^ name$>>
    in

    List.map mkbinding visit_types
    |> reduce (fun members ty ->
        let _loc = Ast.loc_of_rec_binding ty in
        <:rec_binding<$members$; $ty$>>
      )
  in


  let _loc = Loc.ghost in

  let tydcl =
    match kind with
    | Map | Fold ->
        <:str_item<type 'a visitor = { $members$; }>>
    | Iter ->
        <:str_item<type    visitor = { $members$; }>>
  in

  <:str_item<
    open Ast
    open Visitor

    $tydcl$;;

    $functions$;;
    let default = { $default$ };;
  >>
