open Camlp4.PreCast
open DefineOCamlTypes
open PrintOCamlTypes
open ProcessOCamlTypes


let reduce f = function
  | [] -> assert false
  | fn :: fns -> List.fold_left f fn fns


let codegen ocaml_types =
  let ctx = make_context @@ snd @@ List.split ocaml_types in

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
                      <:expr<v.$lid:"fold_" ^ name$ v state $lid:var$>>
                  | Some fn ->
                      let var = mangle name in
                      <:expr<$lid:"fold_" ^ fn$ v.$lid:"fold_" ^ name$ v state $lid:var$>>
                in

                let update =
                  match ty with
                  | ListOfType (_, NamedType (_loc, name))
                    when List.mem name visit_type_names ->
                      Some (mkmap name (Some "list"))
                  | OptionType (_, NamedType (_loc, name))
                    when List.mem name visit_type_names ->
                      Some (mkmap name (Some "option"))
                  | NamedType (_loc, name)
                    when List.mem name visit_type_names ->
                      Some (mkmap name None)
                  | _ ->
                      None
                in

                match update with
                | None -> expr
                | Some update ->
                    <:expr<
                      let state = $update$ in
                      $expr$
                    >>
              ) <:expr<state>>
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

      <:str_item@rec_loc<
        let $lid:"visit_" ^ name$ v state $lid:name$ =
          match $lid:name$.$lid:main_member$ with
          $match_cases$
      >>
    ) visit_types
    |> reduce (fun functions fn ->
        let _loc = Ast.loc_of_str_item fn in
        <:str_item<$functions$;; $fn$>>
      )
  in


  let members =
    let mkty (name, (_loc, _, _), _) =
      <:ctyp<
        $id:(<:ident< $lid:"fold_" ^ name$ >>)$
          : 'a visitor
          -> 'a
          -> $lid:name$
          -> 'a
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
      <:rec_binding<$lid:"fold_" ^ name$ = $lid:"visit_" ^ name$>>
    in

    List.map mkbinding visit_types
    |> reduce (fun members ty ->
        let _loc = Ast.loc_of_rec_binding ty in
        <:rec_binding<$members$; $ty$>>
      )
  in


  let _loc = Loc.ghost in
  <:str_item<
    open Ast
    open Visitor

    type 'a visitor = {
      $members$;
    }

    $functions$;;
    let default = { $default$ };;
  >>
