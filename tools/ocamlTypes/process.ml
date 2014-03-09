(* Common functionality. *)
open Define

module Log = Logger.Make(struct let tag = "ProcessOCamlTypes" end)

let (%) f g x = f (g x)


type context = {
  (* These are actually lookup sets, but since the number of
     types is typically very small, we use simple lists. *)
  enum_types  : string list;
  class_types : string list;
}


let sum_type_is_enum (_, sum_type_name, branches) =
  List.for_all (fun (_, branch_name, types) ->
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
    | SumType (_, name, _ as ty) ->
        if sum_type_is_enum ty then
          (name :: enums, classes)
        else
          (enums, name :: classes)
    | RecordType (_, name, _) ->
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


let make_context ocaml_types =
  let (enum_types, class_types) =
    (* Get all types, including the ones in a recursive definition,
       as a flat list. *)
    flatten_recursive_types ocaml_types
    |> partition_type_names
  in

  (* Extract type names. *)
  { enum_types; class_types; }
