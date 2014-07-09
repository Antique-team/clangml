open Camlp4.PreCast
open OcamlTypes.Sig
open OcamlTypes.Process


type env = {
  mod_name : string;
  simple_name : string;
}


let _loc = Loc.ghost


let rec simplify_basic_type = function
  | NamedType (_loc, name) ->
      <:expr<$lid:"simplify_" ^ name$>>
  | SourceLocation (_loc) ->
      <:expr<Sloc.simplify>>
  | ListOfType (_loc, ty) ->
      <:expr<List.map $simplify_basic_type ty$>>
  | OptionType (_loc, ty) ->
      <:expr<Option.map $simplify_basic_type ty$>>
  | ClangType _ | RefType _ -> assert false


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


let make_simplify_sum_type env filtered_types st =
  <:binding<
    simplify_sum_type = function
      | $uid:env.mod_name$.A -> $uid:env.simple_name$.A
      | $uid:env.mod_name$.B -> $uid:env.simple_name$.B
  >>


let make_simplify_record_type env filtered_types rt =
  let body =
    try

      let main_member = find_composite_member rt in
      <:expr<
        $simplify_basic_type main_member.rtm_type$
          v.$lid:main_member.rtm_name$
      >>

    with Not_found ->

      let rec_bindings =
        List.map (fun member ->
          let _loc = member.rtm_loc in

          let simplify_member =
            simplify_basic_type member.rtm_type
          in

          let target =
            <:ident<$uid:env.simple_name$.$lid:member.rtm_name$>>
          in
          let source =
            <:expr<$simplify_member$ $uid:env.mod_name$.v.$lid:member.rtm_name$>>
          in

          <:rec_binding<$target$ = $source$>>
        ) rt.rt_members
        |> BatList.reduce (fun acc binding ->
             <:rec_binding<$acc$; $binding$>>
           )
      in

      <:expr<{ $rec_bindings$ }>>
  in

  <:binding<
    simplify_record_type v =
      $body$
  >>


let rec make_simplify env filtered_types = function
  | SumType st -> make_simplify_sum_type env filtered_types st
  | RecordType rt -> make_simplify_record_type env filtered_types rt
  | RecursiveType (_loc, types) ->
      types
      (* map *)
      |> List.map (make_simplify env filtered_types)
      (* reduce *)
      |> BatList.reduce (fun acc binding ->
           <:binding<$acc$ and $binding$>>
         )
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

  let code =
    ocaml_types
    |> List.filter (function
         | Version _ -> false
         | _ -> true
       )
    |> List.map (make_simplify env filtered_types)
    |> List.map (fun binding -> <:str_item<let $binding$>>)
    |> BatList.reduce (fun funs fn ->
         <:str_item<$funs$;; $fn$>>
       )
  in

  <:str_item<
    (* For Option.map *)
    open Util
    $code$
  >>
