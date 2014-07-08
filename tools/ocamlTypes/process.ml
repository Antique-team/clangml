(* Common functionality. *)
open Util
open Sig

module Log = Logger.Make(struct let tag = "ProcessOCamlTypes" end)

let (%) f g x = f (g x)


type context = {
  (* These are actually lookup sets, but since the number of
     types is typically very small, we use simple lists. *)
  enum_types  : string list;
  class_types : string list;
}


let sum_type_is_enum { st_name = sum_type_name; st_branches = branches } =
  List.for_all (fun { stb_types = types } ->
    types = []
  ) branches


let type_is_enum = function
  | SumType ty -> sum_type_is_enum ty
  | AliasType _
  | RecordType _ -> false
  | RecursiveType _ -> failwith "type_is_enum cannot handle recursive types"
  | Version _ -> failwith "Version in type list"


(* Partition by enum/class types. *)
let partition_type_names =
  List.fold_left (fun (enums, classes) -> function
    | SumType ({ st_name = name } as ty) ->
        if sum_type_is_enum ty then
          (name :: enums, classes)
        else
          (enums, name :: classes)
    | RecordType { rt_name = name } ->
        (enums, name :: classes)
    | AliasType _ ->
        (enums, classes)
    | Version _ -> Log.err "version does not have a type name"
    | RecursiveType _ -> Log.err "recursive types do not have a type name"
  ) ([], [])


let flatten_recursive_types =
  List.flatten % List.map (function
    | RecursiveType (_, tys) -> tys
    | Version _ -> []
    | ty -> [ty]
  )


let flatten_and_map =
  List.map (function
    | RecordType { rt_name = name }
    | SumType { st_name = name } as ty ->
        (name, ty)
    | _ -> failwith "invalid type returned from flatten_recursive_types"
  ) % flatten_recursive_types


let make_context ocaml_types =
  let (enum_types, class_types) =
    (* Get all types, including the ones in a recursive definition,
       as a flat list. *)
    flatten_recursive_types ocaml_types
    |> partition_type_names
  in

  (* Extract type names. *)
  { enum_types; class_types; }


let make_type_map ocaml_types : type_map list =
  let ctx = make_context @@ snd @@ List.split ocaml_types in

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


let make_visit_type_names type_map =
  List.map
    (fun (name, _, _) -> name)
    type_map


let name_of_type = function
  | SumType { st_name = name }
  | AliasType (_, name, _)
  | RecordType { rt_name = name } -> name
  | Version _ -> failwith "version has no name"
  | RecursiveType _ -> failwith "recursive types have no name"
