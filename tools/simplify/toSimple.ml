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

(* mod_name could be "Ast" for example 
   ocaml_types come form ast.ml
   filtered_types come from astSimple.ml
*)
let codegen mod_name ocaml_types filtered_types =
  <:str_item<
    let simplify = function
      | _ -> assert false
  >>
