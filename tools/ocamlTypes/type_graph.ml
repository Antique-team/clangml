open Sig

module HT = Hashtbl
module P  = Printf

let rec is_visitable_basic_type visited name types = function
  (* non recursive types *)
  | SourceLocation _ -> false
  | ClangType _ -> false
  | RefType _ -> false
  (* maybe recursive types *)
  | ListOfType (_, ty)
  | OptionType (_, ty) ->
      is_visitable_basic_type visited name types ty
  | TupleType (_, tys) ->
    List.exists
      (is_visitable_basic_type visited name types)
      tys
  | NamedType (_, curr_name) ->
      if name = curr_name then
        true
      else
        try
          is_visitable visited types name (List.assoc curr_name types)
        with Not_found -> false

and is_visitable_sum_type_branch
    visited name types { stb_types = basic_types } =

  List.exists
    (is_visitable_basic_type visited name types)
    basic_types

and is_visitable_record_type_member
    visited name types { rtm_type = basic_type } =

  is_visitable_basic_type visited name types basic_type

and is_visitable visited types name ocaml_type =
  if HT.mem visited ocaml_type then
    false
  else
    begin
      HT.add visited ocaml_type ();
      match ocaml_type with
      | SumType { st_branches = sum_type_branches } ->
          List.exists
            (is_visitable_sum_type_branch visited name types)
            sum_type_branches
      | RecordType { rt_members = members } ->
          List.exists
            (is_visitable_record_type_member visited name types)
            members
      | _ -> false
    end;

and must_visit (types : ocaml_types) =
  let res = 
    List.fold_left
      (fun acc (name, ocaml_type) ->
         let visited = HT.create 10 in
         if is_visitable visited types name ocaml_type then
           name :: acc
         else
           acc
      )
      []
      types
  in
  P.printf "[";
  List.iter
    (P.printf "%s; ")
    res;
  P.printf "]\n";
  res
