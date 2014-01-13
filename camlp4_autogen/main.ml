module Log = Logger.Make(struct let tag = "main" end)


(*****************************************************
 * Enum (all tycons are nullary)
 *****************************************************)

let enum_interface_for_constant_sum_type (sum_type_name, branches) =
  (* Explicitly make the first enum have value 0, the second have value 2, etc.*)
  let enum_elements =
    List.mapi (fun i (branch_name, _) ->
      (branch_name, Some i)
    ) branches
  in

  Codegen.({
    enum_name = sum_type_name;
    enum_elements = enum_elements;
  })


let sum_type_is_enum (sum_type_name, branches) =
  List.for_all (fun (branch_name, types) ->
    types = []
  ) branches


(*****************************************************
 * Class (none of the tycons are nullary)
 *****************************************************)


let class_interface_for_sum_type (sum_type_name, branches) =
  failwith (Show.show_list<Parse.sum_type_branch> branches);
  (* TODO: Implement me! *)
  Log.unimp "Non-enum type in process_sum_type"


(*****************************************************
 * Main
 *****************************************************)

let gen_code_for_sum_type (sum_type : Parse.sum_type) =
  if sum_type_is_enum sum_type then
    Codegen.Enum (enum_interface_for_constant_sum_type sum_type)
  else
    Codegen.Enum (class_interface_for_sum_type sum_type)


let gen_code_for_ocaml_type = function
  | Parse.SumType sum_type ->
      [gen_code_for_sum_type sum_type]
  | Parse.RecursiveType rec_types ->
      List.map gen_code_for_sum_type rec_types


let code_gen (sum_types : Parse.ocaml_type list) =
  let cpp_types =
    List.map gen_code_for_ocaml_type sum_types
    |> List.flatten
  in
  let cg = Codegen.make_codegen_with_channel stdout in
  List.iter (Codegen.emit_interface cg) cpp_types


let parse_and_generate filename =
  let sum_types = Parse.parse_file filename in
  print_endline (Show.show_list<Parse.ocaml_type> sum_types);
  code_gen sum_types

  (* TODO: Think about detecting versioning mismatch between generated and ocaml ast.*)
  (* Maybe keep a version number around someplace?*)


let () =
  parse_and_generate "hello_ast.ml"
