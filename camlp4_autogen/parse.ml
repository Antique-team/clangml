open Camlp4.PreCast

let (%) f g x = f (g x)

(* The P4 AST is specified and sort of documented where it is defined.*)
(* See: https://github.com/def-lkb/ocaml-tyr/blob/8e1296b0802c28b99d16b975310c8abbcd76e9e9/camlp4/Camlp4/Camlp4Ast.partial.ml*)
(* *)
(* *)

module M = Camlp4OCamlRevisedParser.Make(Syntax)
module P4Parser = Camlp4OCamlParser.Make(Syntax)
(* DCC: Note: above are needed for their side effects (grrr) *)

module Quotation = Camlp4QuotationCommon.Make(P4Parser)(Syntax.AntiquotSyntax)
module P4Printer = Camlp4.Printers.OCaml.Make(Syntax)

module Log = Logger.Make(struct let tag = "parse" end)


type basic_type =
  (* Simple type *)
  | NamedType of string
  (* Clang pointer *)
  | ClangType of string
  (* List of basic_type *)
  | ListOfType of basic_type
  (* Optional basic_type *)
  | OptionType of basic_type
  (* Will want others, eventually *)
  deriving (Show)

type sum_type_branch = (* branch_name *)string * (* types *)basic_type list
  deriving (Show)

type sum_type = (* sum_type_name *)string * (* branches *)sum_type_branch list
  deriving (Show)

type record_member = (* name *)string * (* type *)basic_type
  deriving (Show)

type record_type = (* name *)string * (* members *)record_member list
  deriving (Show)

type ocaml_type =
  | SumType of sum_type
  | RecordType of record_type
  | RecursiveType of ocaml_type list
  | Version of string
  deriving (Show)


(* Debugging *)

let print_meta_expr (e : Ast.expr) =
  P4Printer.print None (fun o -> o#expr) e;
  print_endline ""

let print_str_item_as_meta (str_item : Ast.str_item) : unit =
  let meta_e = Quotation.MetaAst.Expr.meta_str_item Ast.Loc.ghost str_item in
  print_meta_expr meta_e

let print_ctyp (t : Ast.ctyp) =
  P4Printer.print None (fun o f t -> Format.fprintf f "@[<v2>ctyp: <%a>@]@\n" o#ctyp t) t

let print_ctyp_as_meta (t : Ast.ctyp) : unit =
  let meta_t = Quotation.MetaAst.Expr.meta_ctyp Ast.Loc.ghost t in
  print_meta_expr meta_t

let print_expanded_str_item str_item =
  let e = Quotation.MetaAst.Expr.meta_str_item Ast.Loc.ghost str_item in
  print_meta_expr e



let rec ast_type_to_type (ctyp: Ast.ctyp) =
  match ctyp with
  | <:ctyp<$lid:identifier$>> ->
      NamedType (identifier)

  | <:ctyp<list $ty$>> ->
      let list_of_type = ast_type_to_type ty in
      ListOfType (list_of_type)

  | <:ctyp<option $ty$>> ->
      let list_of_type = ast_type_to_type ty in
      OptionType (list_of_type)

  | <:ctyp<Clang.$uid:node_type$.t>> ->
      ClangType (node_type)

  | _ ->
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
  let (identifier, ast_branch_components) =
    match ctyp with
    | Ast.TyId (_, Ast.IdUid (_, identifier)) ->
        (identifier, [])
    | Ast.TyOf (_, Ast.TyId (_, Ast.IdUid (_, identifier)), of_components) ->
        let n_ary_components =
          match of_components with
          | Ast.TyAnd _ ->
              (* Multiple components *)
              flatten_ast_tuple_type_components of_components
          | _ ->
              (* Single component *)
              [of_components]
        in
        (identifier, n_ary_components)
    | _ -> Log.err "Unhandled sum type branch"
  in
  (identifier, List.map ast_type_to_type ast_branch_components)


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
  | <:ctyp<$lid:name$ : $ty$>> ->
      (name, ast_type_to_type ty)
  | ty ->
      Log.err "unhandled record member format"


let map_record_members =
  List.rev_map map_record_member
  % flatten_ast_record_members []


let map_rec_type = function
  | Ast.TyDcl (_, name, [], <:ctyp<[ $ast_branches$ ]>>, []) ->
      SumType (name, map_sum_type ast_branches)
  | Ast.TyDcl (_, name, [], <:ctyp<{ $members$ }>>, []) ->
      RecordType (name, map_record_members members)
  | ty ->
      (* Perhaps ignore this instead. *)
      Log.unimp "only sum types are supported"


let ast_str_item_to_sum_type types (str_item : Ast.str_item) : ocaml_type list =
  match str_item with
  | <:str_item<type $lid:name$ = [ $ast_branches$ ]>> ->
      SumType (name, map_sum_type ast_branches) :: types

  | <:str_item<type $lid:name$ = { $members$ }>> ->
      RecordType (name, map_record_members members) :: types

  | <:str_item<type $ty1$ and $ty2$>> ->
      let flattened = flatten_ast_rec_types [ty2] ty1 in
      let rec_types = List.map map_rec_type flattened in
      RecursiveType (rec_types) :: types

  | <:str_item<value version = $str:version$>> ->
      Version version :: types

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
