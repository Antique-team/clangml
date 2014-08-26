open Util
open OcamlTypes.Sig
open OcamlTypes.Process

module Log = Logger.Make(struct let tag = "main" end)

let (%) f g x = f (g x)


(**
  Turn an underscore_name into a CamelcaseName.
  This function is non-destructive (returns a new string).
 *)
let cpp_name name =
  let rec to_camelcase length name =
    try
      (* If the name starts with an underscore,
         keep that first underscore. *)
      let underscore = String.index_from name 1 '_' in
      (* If it ends with one, also keep that. *)
      if underscore = length - 1 then
        raise Not_found
      else (
        String.blit
          name (underscore + 1)
          name underscore
          (length - underscore - 1);
        name.[underscore] <- Char.uppercase name.[underscore];
        to_camelcase (length - 1) name
      )
    with Not_found ->
      (* Second copy here. *)
      String.sub name 0 length
  in
  (* First copy here. *)
  to_camelcase (String.length name) (String.capitalize name)


(*****************************************************
 * Enum (all tycons are nullary)
 *****************************************************)

let enum_intf_for_constant_sum_type { st_name = sum_type_name;
                                      st_branches = branches } =
  (* Explicitly make the first enum have value 0, the second have value 2, etc.*)
  let enum_elements =
    List.mapi (fun i { stb_name = branch_name } ->
      (branch_name, Some i)
    ) branches
  in

  Codegen.({
    enum_name = cpp_name sum_type_name;
    enum_elements = enum_elements;
  })


(*****************************************************
 * Class (some of the tycons have arguments)
 *****************************************************)


let is_basic_type = function
  | "bool"
  | "char"
  | "int"
  | "float"
  | "string" ->
      true
  | _ ->
      false


let is_nonnull_ptr ctx = function
  | NamedType (_, name) ->
      (* Basic types and enum types are no pointers. *)
      if is_basic_type name || List.mem name ctx.enum_types then
        false
      else
        true
  | _ ->
      (* Clang and option types are nullable and list types are no pointers. *)
      false


let rec translate_type ctx = let open Codegen in function
  | NamedType (_, "bool") ->
      TyBool
  | NamedType (_, "char") ->
      TyUChar
  | NamedType (_, "int") ->
      TyInt
  | NamedType (_, "float") ->
      TyFloat
  | NamedType (_, "string") ->
      TyString
  | NamedType (_, name) ->
      let ty = TyName (cpp_name name) in
      (* Make sure enum/constant ADTs are passed and stored
         by value, not by pointer. *)
      if List.mem name ctx.enum_types then
        ty
      else if List.mem name ctx.class_types then
        (* Automatic memory management in C++ bridge. *)
        TyTemplate ("ptr", [ty])
      else (
        Log.warn "Name '%s' is not an ADT in the same file"
          name;
        (* Plain pointers to anything unknown. *)
        TyPointer (ty)
      )
  | RefType (_, ty) ->
      (* Plain pointers to Clang AST nodes. *)
      TyTemplate ("recursive_ptr", [translate_type ctx ty])
  | ClangType (_, name) ->
      (* Plain pointers to Clang AST nodes. *)
      TyTemplate ("clang_ref", [TyName (cpp_name name)])
  | SourceLocation (_) ->
      TyName "clang::SourceLocation"
  | ListOfType (_, ty) ->
      TyTemplate ("std::vector", [translate_type ctx ty])
  | OptionType (_, (NamedType (_, name) as ty)) ->
      let ty =
        if is_basic_type name then
          translate_type ctx ty
        else
          TyName (cpp_name name)
      in
      TyTemplate ("option", [ty])
  | OptionType (_, ty) ->
      TyTemplate ("option", [translate_type ctx ty])
  | TupleType (_, tys) ->
      TyTemplate ("std::tuple", List.map (translate_type ctx) tys)


(* Constructor arguments. *)
let constructor_params ctx types =
  let open Codegen in
  List.mapi (fun i ty ->
    { empty_decl with
      decl_type = translate_type ctx ty;
      decl_name = "arg" ^ string_of_int i;
    }
  ) types


let int_const_fn ty name value =
  let open Codegen in
  MemberFunction {
    flags  = [Virtual];
    retty  = TyName ty;
    name   = name;
    params = [];
    this_flags = [Const];
    body   = CompoundStmt [
      Return (IntLit value)
    ];
  }

let tag_const  = int_const_fn "tag_t"    "tag"
let size_const = int_const_fn "mlsize_t" "size"


let toValue fields =
  let open Codegen in

  let body =
    CompoundStmt [
      Return (
        FCall (
          "value_of_adt",
          IdExpr "ctx"
          :: IdExpr "this"
          :: fields
        )
      );
    ]
  in
  MemberFunction {
    flags  = [Virtual];
    retty  = TyName "value";
    name   = "ToValue";
    params = [{
      empty_decl with
      decl_type = TyReference (TyName "value_of_context");
      decl_name = "ctx";
    }];
    this_flags = [Const];
    body;
  }


let class_intf_for_sum_type ctx { st_name = sum_type_name;
                                  st_branches = branches; } =
  let open Codegen in

  let class_name = cpp_name sum_type_name in

  (* Base class *)
  let base = {
    class_name;
    class_bases = ["OCamlADT<" ^ class_name ^ ">"];
    class_fields = [];
    class_methods = [];
  } in

  let derived =
    let explicit = [Explicit] in

    let make_derived tag { stb_name = branch_name; stb_types = types; } =
      let toValue =
        toValue (List.mapi (fun i _ -> IdExpr ("field" ^ string_of_int i)) types)
      in

      let constructor =
        (* Constructor initialiser list. *)
        let init =
          List.mapi (fun i _ ->
            ("field" ^ string_of_int i, "arg" ^ string_of_int i)
          ) types
        in

        let assert_fn i =
          Expression (FCall ("assert", [IdExpr ("arg" ^ string_of_int i)]))
        in

        let body =
          (* Enforce invariants. *)
          CompoundStmt (List.mapi (fun i ty ->
            if is_nonnull_ptr ctx ty then
              [assert_fn i]
            else
              []
          ) types |> List.flatten)
        in

        let params = constructor_params ctx types in

        let flags =
          (* Explicit constructor only if it's a unary constructor. *)
          if List.length params = 1 then
            explicit
          else
            []
        in
        MemberConstructor (flags, params, init, body)
      in

      let fields =
        List.mapi (fun i ty ->
          MemberField {
            empty_decl with
            decl_type = translate_type ctx ty;
            decl_name = "field" ^ string_of_int i;
          }
        ) types
      in

      {
        class_name = branch_name ^ base.class_name;
        class_bases = [base.class_name];
        class_fields = fields;
        class_methods = [
          size_const (List.length types);
          tag_const tag;
          toValue;
          constructor;
        ]
      }
    in

    let (nullary, parameterised) =
      List.partition
        (fun stb -> stb.stb_types = [])
        branches
    in
    (* First, nullary constructors. *)
    List.mapi make_derived nullary
    @ (* Then, tycons with arguments. *)
    List.mapi make_derived parameterised
  in
  (base, derived)


(*****************************************************
 * Main
 *****************************************************)

(* Returns (enums+bases, records, deriveds).
   This function only returns enums+bases and deriveds. *)
let gen_code_for_sum_type ctx (sum_type : sum_type) =
  if List.mem sum_type.st_name ctx.enum_types then
    ([Codegen.Enum (enum_intf_for_constant_sum_type sum_type)], [], [])
  else
    (* Types. *)
    let base, deriveds =
      class_intf_for_sum_type ctx sum_type
    in

    let mkclass i = Codegen.Class i in

    let base = mkclass base in
    let deriveds =
      List.map mkclass deriveds

      (* mk* object creation functions. *)
      @ List.map (fun { stb_name = branch_name; stb_types = types; } ->
          let open Codegen in
          let ty =
            TyName (branch_name ^
                    cpp_name sum_type.st_name)
          in
          let args =
            List.mapi (fun i _ ->
              IdExpr ("arg" ^ string_of_int i)
            ) types
          in
          Function {
            flags  = [Static; Inline];
            retty  = TyTemplate ("ptr", [ty]);
            name   = "mk" ^ branch_name;
            params = constructor_params ctx types;
            this_flags = [];
            body   = CompoundStmt [
              Return (
                New (ty, args)
              )
            ];
          }
        ) sum_type.st_branches

    in

    ([base], [], deriveds)


let class_intf_for_record_type ctx { rt_name = record_name;
                                     rt_members = fields; } =
  let open Codegen in

  let class_fields =
    List.map (fun { rtm_name = name; rtm_type = ty; } ->
      MemberField {
        empty_decl with
        decl_type = translate_type ctx ty;
        decl_name = name;
      }
    ) fields
  in

  let toValue =
    toValue (List.map (fun rtm -> IdExpr (rtm.rtm_name)) fields)
  in

  let class_name = cpp_name record_name in

  {
    class_name;
    class_bases = ["OCamlADT<" ^ class_name ^ ">"];
    class_fields;
    class_methods = [size_const (List.length fields); tag_const 0; toValue];
  }


(* Returns (enums+bases, records, deriveds).
   This one only adds records. *)
let gen_code_for_record_type ctx (record_type : record_type) =
  let open Codegen in

  let factory =
    let class_name = cpp_name record_type.rt_name in
    let ty = TyName class_name in
    Function {
      flags  = [Static; Inline];
      retty  = TyTemplate ("ptr", [ty]);
      name   = "mk" ^ class_name;
      params = [];
      this_flags = [];
      body   = CompoundStmt [
        Return (
          New (ty, [])
        )
      ];
    }
  in

  ([], [Codegen.Class (class_intf_for_record_type ctx record_type); factory], [])


let gen_code_for_ocaml_type ctx = function
  | SumType ty -> gen_code_for_sum_type ctx ty
  | RecordType ty -> gen_code_for_record_type ctx ty
  | AliasType _ -> ([], [], [])
  | Version _ -> Log.err "version in recursive type"
  | RecursiveType _ -> Log.err "recursive type in recursive type"


let gen_code_for_rec_type ctx = function
  | RecursiveType (_, rec_types) ->
      (* Generate forward declarations for recursive types.
         Only consider types that will be turned into classes,
         as recursive type definitions may contain some enum
         types, as well (even though that would be useless). *)
      List.map (fun sum_type ->
        Codegen.Forward (cpp_name @@ name_of_type sum_type)
      ) (List.filter (not % type_is_enum) rec_types)
      @
      (
        List.rev_map (gen_code_for_ocaml_type ctx) rec_types
        |> List.fold_left
             (fun (bases, records, deriveds) (b, r, d) ->
               (b @ bases, r @ records, d @ deriveds)
             )
             ([], [], [])
        |> (fun (bases, records, deriveds) ->
              bases @ records @ deriveds)
      )
  | Version (_, version) ->
      let open Codegen in
      let ty = TyConst (TyPointer (TyConst TyChar)) in
      [Variable (ty, "version", StrLit version)]
  | ty ->
      let (bases, records, deriveds) = gen_code_for_ocaml_type ctx ty in
      bases @ records @ deriveds


let code_gen dir basename (ocaml_types : ocaml_type list) =
  let ctx = make_context ocaml_types in

  let cpp_types =
    List.map (gen_code_for_rec_type ctx) ocaml_types
    |> List.flatten
  in
  begin (* interface *)
    let fh = open_out (dir ^ "/" ^ basename ^ ".h") in
    let cg = Codegen.make_codegen_with_channel fh in
    Codegen.emit_intfs basename cg cpp_types;
    Codegen.flush cg;
    close_out fh;
  end;
  begin (* implementation *)
    let fh = open_out (dir ^ "/" ^ basename ^ ".cpp") in
    let cg = Codegen.make_codegen_with_channel fh in
    Codegen.emit_impls basename cg cpp_types;
    Codegen.flush cg;
    close_out fh;
  end;
;;


let parse_and_generate dir basename source =
  let ocaml_types = OcamlTypes.Parse.parse_file source in

  (* Get AST version. *)
  begin
    match
      List.map (function
        | Version (_, version) -> [version]
        | _ -> []
      ) ocaml_types
      |> List.flatten
    with
    | [version] -> ()
    | [] ->
        Log.err "No version found"
    | versions ->
        Log.err "Multiple versions found: [%a]"
          (Formatx.pp_list Formatx.pp_print_string) versions
  end;

  (*print_endline (Show.show_list<ocaml_type> ocaml_types);*)
  code_gen dir basename ocaml_types

  (* TODO: Think about detecting versioning mismatch between generated and ocaml ast.*)
  (* Maybe keep a version number around someplace?*)


let () =
  match Sys.argv with
  | [|_; dir; basename; source|] ->
      parse_and_generate dir basename source
  | _ ->
      print_endline "Usage: bridgen <output-path> <basename> <source>"
