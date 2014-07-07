open Camlp4.PreCast
open Util
open Sig
open Print

let (%) f g x = f (g x)

(* The P4 AST is specified and sort of documented where it is defined.*)
(* See: https://github.com/def-lkb/ocaml-tyr/blob/8e1296b0802c28b99d16b975310c8abbcd76e9e9/camlp4/Camlp4/Camlp4Ast.partial.ml*)
(* *)
(* *)

module Log = Logger.Make(struct let tag = "parse" end)


let rec ast_type_to_type (ctyp: Ast.ctyp) =
  match ctyp with
  | <:ctyp@loc<$lid:identifier$>> ->
      NamedType (loc, identifier)

  | <:ctyp@loc<list $ty$>> ->
      let list_of_type = ast_type_to_type ty in
      ListOfType (loc, list_of_type)

  | <:ctyp@loc<option $ty$>> ->
      let list_of_type = ast_type_to_type ty in
      OptionType (loc, list_of_type)

  | <:ctyp@loc<Util.DenseIntMap.key $ty$>> ->
      let ref_type = ast_type_to_type ty in
      RefType (loc, ref_type)

  | <:ctyp@loc<Ref.t $lid:node_type$>> ->
      ClangType (loc, node_type)

  | <:ctyp@loc<Sloc.t>> ->
      SourceLocation (loc)

  | ty ->
      Print.print_ctyp ty;
      Log.unimp "ast_type_to_type"


(* Flatten tree of Ast.TyOrs of types to list of types *)
let flatten_ast_sum_type_branches = function
  | Ast.TyOr _ as t ->
      let rec flatten_sum_type_contents_aux t flattened_contents =
        match t with
        | Ast.TyOr (_, left, right) ->
            flattened_contents
            |> flatten_sum_type_contents_aux left
            |> flatten_sum_type_contents_aux right
        | t ->
            t :: flattened_contents
      in
      flatten_sum_type_contents_aux t []
  | t ->
      Log.warn "Expected sum type contents to be Ast.TyOr";
      [t]


(* Flatten tree of Ast.TyAnds of types to list of types *)
(* TODO: Consider factoring out commonalities with flatten_ast_sum_type_branches*)
(* via some kind of very general tree fold. Or not.*)
let flatten_ast_tuple_type_components (t : Ast.ctyp) : Ast.ctyp list =
  match t with
  | Ast.TyAnd _ ->
      begin
        let rec flatten_tuple_type_contents_aux t flattened_contents =
          match t with
          | Ast.TyAnd (_, left, right) ->
              flattened_contents
              |> flatten_tuple_type_contents_aux right
              |> flatten_tuple_type_contents_aux left
          | _  -> t :: flattened_contents
        in
        flatten_tuple_type_contents_aux t []
      end
  | _ -> Log.err "Expected tuple type contents to be Ast.TyAnd"


let ast_sum_type_branch_to_branch (ctyp: Ast.ctyp) : sum_type_branch =
  let (loc, identifier, ast_branch_components) =
    match ctyp with
    | Ast.TyId (loc, Ast.IdUid (_, identifier)) ->
        (loc, identifier, [])
    | Ast.TyOf (loc, Ast.TyId (_, Ast.IdUid (_, identifier)), of_components) ->
        let n_ary_components =
          match of_components with
          | Ast.TyAnd _ ->
              (* Multiple components *)
              flatten_ast_tuple_type_components of_components
          | _ ->
              (* Single component *)
              [of_components]
        in
        (loc, identifier, n_ary_components)
    | _ -> Log.err "Unhandled sum type branch"
  in
  {
    stb_loc = loc;
    stb_name = identifier;
    stb_types = List.map ast_type_to_type ast_branch_components;
  }


let rec flatten_ast_rec_types list = function
  | <:ctyp<$ty1$ and $ty2$>> ->
      flatten_ast_rec_types (ty2 :: list) ty1
  | ty ->
      ty :: list


let rec flatten_ast_record_members list = function
  | <:ctyp<$decl$; $rest$>> ->
      flatten_ast_record_members (decl :: list) rest
  | ty ->
      ty :: list


let map_sum_type =
  List.rev_map ast_sum_type_branch_to_branch
  % flatten_ast_sum_type_branches


let map_record_member = function
  | <:ctyp@loc<$lid:name$ : $ty$>> ->
      {
        rtm_loc = loc;
        rtm_name = name;
        rtm_type = ast_type_to_type ty;
      }
  | ty ->
      Log.err "unhandled record member format"


let map_record_members =
  List.rev_map map_record_member
  % flatten_ast_record_members []


let map_rec_type = function
  | Ast.TyDcl (loc, name, [], <:ctyp<[ $ast_branches$ ]>>, []) ->
      SumType {
        st_loc = loc;
        st_name = name;
        st_branches = map_sum_type ast_branches;
      }
  | Ast.TyDcl (loc, name, [], <:ctyp<{ $members$ }>>, []) ->
      RecordType {
        rt_loc = loc;
        rt_name = name;
        rt_members = map_record_members members;
      }
  | Ast.TyDcl (loc, name, [], other, []) ->
      AliasType (loc, name, ast_type_to_type other)
  | ty ->
      (* Perhaps ignore this instead. *)
      Log.unimp "only sum types are supported"


let ast_str_item_to_sum_type types (str_item : Ast.str_item) : ocaml_type list =
  match str_item with
  | <:str_item@loc<type $lid:name$ = [ $ast_branches$ ]>> ->
      SumType {
        st_loc = loc;
        st_name = name;
        st_branches = map_sum_type ast_branches;
      } :: types

  | <:str_item@loc<type $lid:name$ = { $members$ }>> ->
      RecordType {
        rt_loc = loc;
        rt_name = name;
        rt_members = map_record_members members;
      } :: types

  | <:str_item@loc<type $ty1$ and $ty2$>> ->
      let flattened = flatten_ast_rec_types [ty2] ty1 in
      let rec_types = List.map map_rec_type flattened in
      RecursiveType (loc, rec_types) :: types

  | <:str_item@loc<value version = $str:version$>> ->
      Version (loc, version) :: types

  | _ ->
      types


let parse_file (file_path : string) : ocaml_type list =
  let st = Stream.of_channel (open_in file_path) in
  let str_item = Syntax.parse_implem (Loc.mk file_path) st in
  let str_items = Ast.list_of_str_item str_item [] in

  (*	List.iter Printers.OCaml.print_implem str_items ;
  (*print_endline "Printing expanded" ; *)
  List.iter print_expanded_str_item str_items;
  List.iter process_str_item str_items
  *)
  let accumulate_sum_type
      (str_item  : Ast.str_item)
      (sum_types : ocaml_type list)
    : ocaml_type list =
    ast_str_item_to_sum_type sum_types str_item
  in

  List.fold_right accumulate_sum_type str_items []
