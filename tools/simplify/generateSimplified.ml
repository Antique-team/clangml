open Camlp4.PreCast
open OcamlTypes.Sig
open OcamlTypes.Process

module S = BatString

let codegen ocaml_types =
  let _loc = Loc.ghost in

  let filtered_types =
    let rec contains_type name = function
      | [] -> false
      | ty :: rest ->
          (match ty with
           | SumType    { st_name = ty_name }
           | RecordType { rt_name = ty_name } ->
               ty_name = name
           | RecursiveType (_, types) ->
               contains_type name types
           | AliasType _
           | Version _ ->
               false
          ) || contains_type name rest
    in

    let has_child_type name =
      contains_type (name ^ "_") ocaml_types
    in

    let rec remove_combined_types types =
      List.fold_right
        (fun ty filtered_types ->
           match ty with
           | SumType    st when has_child_type st.st_name ->
               filtered_types
           | RecordType rt when has_child_type rt.rt_name ->
               filtered_types
           | RecursiveType (loc, types) ->
               RecursiveType (loc, remove_combined_types types)
                 :: filtered_types
           | ty ->
               ty :: filtered_types
        )
        types
        []
    in

    remove_combined_types ocaml_types
  in

  let filtered_types =
    let remove_underscore name =
      if S.ends_with name "_" then
        S.rchop name
      else
        name
    in

    let rec rename_type = function
      | SumType st ->
          SumType {
            st with st_name = remove_underscore st.st_name;
          }
      | RecordType rt ->
          RecordType {
            rt with rt_name = remove_underscore rt.rt_name;
          }
      | RecursiveType (loc, types) ->
          RecursiveType (loc, List.map rename_type types)
      | ty -> ty
    in

    List.map rename_type filtered_types
  in

  (OcamlTypes.Codegen.codegen filtered_types, filtered_types)
