open Camlp4.PreCast
open OcamlTypes.Sig


let _loc = Loc.ghost


(*
let simplify_language = function
  | Ast.Lang_C -> AstSimple.Lang_C
  | Ast.Lang_CXX -> AstSimple.Lang_CXX

let simplify_type_specifier_width = function
  | Ast.TSW_unspecified -> AstSimple.TSW_unspecified
  | Ast.TSW_short -> AstSimple.TSW_short
  | Ast.TSW_long -> AstSimple.TSW_long
  | Ast.TSW_longlong -> AstSimple.TSW_longlong

(* more enums *)

let simplify_written_builtin_specs written_builtin_specs =
  {
    AstSimple.wbs_type = written_builtin_specs.Ast.wbs_type;
    AstSimple.wbs_sign = written_builtin_specs.Ast.wbs_sign;
    AstSimple.wbs_width = written_builtin_specs.Ast.wbs_width;
    AstSimple.wbs_mode_attr = written_builtin_specs.Ast.wbs_mode_attr;
  }

(* more enums, more records *)

let rec simplify_desg desg =
  simplify_desg_ desg.Ast.dr

and simplify_desg_ = function
  | Ast.FieldDesignator             string ->
      AstSimple.FieldDesignator (string)
  | Ast.ArrayDesignator             expr0 ->
      let expr0 = simplify_expr expr0 in
      AstSimple.ArrayDesignator (expr0)
  | Ast.ArrayRangeDesignator        (expr0, expr1) ->
      let expr0 = simplify_expr expr0 in
      let expr1 = simplify_expr expr1 in
      AstSimple.ArrayRangeDesignator (expr0. expr1)



and simplify_expr expr =
  simplify_expr_ expr.Ast.dr

and simplify_expr_ = function
  | Ast.IntegerLiteral i _> AstSimple.IntegerLiteral i
  | _ -> ...
*)


let make_simplify_sum_type mod_name filtered_types st =
  <:str_item<
    let simplify_sum_type = function
      | $uid:mod_name$.A -> $uid:mod_name ^ "Simple"$.A
      | $uid:mod_name$.B -> $uid:mod_name ^ "Simple"$.B
  >>


let make_simplify_record_type mod_name filtered_types rt =
  <:str_item<
    let simplify_record_type rt =
      {
        $uid:mod_name ^ "Simple"$.field1 = $uid:mod_name$.field1;
        $uid:mod_name ^ "Simple"$.field2 = simplify_sum_type $uid:mod_name$.field2;
      }
  >>


let rec make_simplify mod_name filtered_types = function
  | SumType st -> make_simplify_sum_type mod_name filtered_types st
  | RecordType rt -> make_simplify_record_type mod_name filtered_types rt
  | RecursiveType (_loc, types) ->
      (* map *)
      let funs = List.map (make_simplify mod_name filtered_types) types in
      (* reduce *)
      <:str_item<let recursive_simplify () = ()>>
  | Version _
  | AliasType _ -> assert false


(* mod_name could be "Ast" for example 
   ocaml_types come form ast.ml
   filtered_types come from astSimple.ml
*)
let codegen mod_name filtered_types ocaml_types =
  ocaml_types
  |> List.filter (function
       | Version _ -> false
       | _ -> true
     )
  |> List.map (make_simplify mod_name filtered_types)
  |> BatList.reduce (fun funs fn ->
       <:str_item<$funs$;; $fn$>>
     )
