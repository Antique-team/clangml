open Camlp4.PreCast
open OcamlTypes.Sig


type env = {
  mod_name : string;
  simple_name : string;
}


let _loc = Loc.ghost


(*

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

(*
let simplify_language = function
  | Ast.Lang_C -> AstSimple.Lang_C
  | Ast.Lang_CXX -> AstSimple.Lang_CXX
*)
let make_simplify_sum_type env filtered_types st =
  let fun_name = "simplify_" ^ st.st_name in
  let branches =
    List.map
      (fun b ->
         <:match_case<$uid:env.mod_name$.$uid:b.stb_name$
         ->
         $uid:env.simple_name$.$uid:b.stb_name$>>
      )
      st.st_branches
    |> BatList.reduce (fun acc ty -> <:match_case<$acc$ | $ty$>>)
  in
  <:str_item<
    let $lid:fun_name$ = function $branches$
  >>


let make_simplify_record_type env filtered_types rt =
  <:str_item<
    let simplify_record_type rt =
      {
        $uid:env.simple_name$.field1 = $uid:env.mod_name$.field1;
        $uid:env.simple_name$.field2 = simplify_sum_type $uid:env.mod_name$.field2;
      }
  >>


let rec make_simplify env filtered_types = function
  | SumType st -> make_simplify_sum_type env filtered_types st
  | RecordType rt -> make_simplify_record_type env filtered_types rt
  | RecursiveType (_loc, types) ->
      (* map *)
      let funs = List.map (make_simplify env filtered_types) types in
      (* reduce *)
      <:str_item<let recursive_simplify () = ()>>
  | Version _
  | AliasType _ -> assert false


(* mod_name could be "Ast" for example
   ocaml_types come form ast.ml
   filtered_types come from astSimple.ml
*)
let codegen mod_name filtered_types ocaml_types =
  let env = {
    mod_name;
    simple_name = mod_name ^ "Simple";
  } in

  ocaml_types
  |> List.filter (function
       | Version _ -> false
       | _ -> true
     )
  |> List.map (make_simplify env filtered_types)
  |> BatList.reduce (fun funs fn ->
       <:str_item<$funs$;; $fn$>>
     )
