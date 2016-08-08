open Camlp4.PreCast
open OcamlTypes.Sig
open OcamlTypes.Process
open Util.Prelude


type env = {
  mod_name : string;
  simple_name : string;
}


let rec simplify_basic_type = function
  | NamedType (_loc, name) ->
      <:expr<$lid:"simplify_" ^ name$>>
  | SourceLocation (_loc) ->
      <:expr<Simplify.simplify_sloc>>
  | ListOfType (_loc, ty) ->
      <:expr<simplify_list $simplify_basic_type ty$>>
  | OptionType (_loc, ty) ->
      <:expr<simplify_option $simplify_basic_type ty$>>
  | TupleType (_loc, [ty1; ty2]) ->
      <:expr<simplify_tuple2 $simplify_basic_type ty1$ $simplify_basic_type ty2$>>
  | TupleType (_loc, [ty1; ty2; ty3]) ->
      <:expr<simplify_tuple3 $simplify_basic_type ty1$ $simplify_basic_type ty2$ $simplify_basic_type ty3$>>
  | TupleType (_loc, tys) ->
      abort (Log.fatal "unimp: tuples with more than 3 elements")
  | ClangType _ | RefType _ -> assert false


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

let make_match_case env branch =
  let _loc = branch.stb_loc in
  let tycon = branch.stb_name in

  (* Generic construction function for patterns and expressions. *)
  let construct init mkty reduce =
    List.mapi
      (fun i ty ->
         let _loc = loc_of_basic_type_name ty in
         let name = name_of_basic_type ty in
         _loc, mkty i _loc name
      )
      branch.stb_types
    |> List.fold_left
         (fun tycon (_loc, param) -> reduce _loc tycon param)
         init
  in

  let update =
    (* Reconstruction expression. *)
    let result =
      construct <:expr<$uid:env.simple_name$.$uid:tycon$>>
        (fun i _loc name ->
           <:expr<$lid:name ^ string_of_int i$>>)
        (fun _loc tycon param ->
           Ast.ExApp (_loc, tycon, param))
    in

    (* Simplify calls. *)
    List.mapi (fun i ty -> (i, ty)) branch.stb_types
    |> List.rev
    |> List.fold_left (fun expr (i, ty) ->
         let name = name_of_basic_type ty ^ string_of_int i in
         let _loc = loc_of_basic_type_name ty in
         <:expr<
           let $lid:name$ = $simplify_basic_type ty$ $lid:name$ in
           $expr$
         >>
       ) result

  (* Matching pattern. *)
  and pattern =
    construct <:patt<$uid:env.mod_name$.$uid:tycon$>>
      (fun i _loc name ->
         <:patt<$lid:name ^ string_of_int i$>>)
      (fun _loc tycon param ->
         Ast.PaApp (_loc, tycon, param))
  in

  <:match_case<$pattern$ -> $update$>>

(*
let simplify_language = function
  | Ast.Lang_C -> AstSimple.Lang_C
  | Ast.Lang_CXX -> AstSimple.Lang_CXX
*)
let make_simplify_sum_type env filtered_types st =
  let _loc = st.st_loc in

  let fun_name = "simplify_" ^ st.st_name in
  let branches =
    List.map
      (make_match_case env)
      st.st_branches
    |> BatList.reduce (fun acc ty -> <:match_case<$acc$ | $ty$>>)
  in

  <:binding<
    $lid:fun_name$ = function $branches$
  >>


let make_simplify_record_type env filtered_types rt =
  let _loc = rt.rt_loc in

  let body =
    try

      let main_member = find_composite_member rt in
      <:expr<
        $simplify_basic_type main_member.rtm_type$
          v.$uid:env.mod_name$.$lid:main_member.rtm_name$
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
            <:expr<$simplify_member$ v.$uid:env.mod_name$.$lid:member.rtm_name$>>
          in

          <:rec_binding<$target$ = $source$>>
        ) rt.rt_members
        |> BatList.reduce (fun acc binding ->
             <:rec_binding<$acc$; $binding$>>
           )
      in

      <:expr<{ () with $rec_bindings$ }>>
  in

  <:binding<
    $lid:"simplify_" ^ rt.rt_name$ v =
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
  let _loc = Loc.ghost in

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
    |> List.map (fun binding -> <:str_item<let rec $binding$>>)
    |> BatList.reduce (fun funs fn ->
         <:str_item<$funs$;; $fn$>>
       )
  in

  <:str_item<
    (* For simplify_list/bool/string/option/... *)
    open Simplify
    $code$
  >>
