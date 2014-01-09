
module Log = Logger.Make(struct let tag = "main" end)

let rec parse_and_generate filename = 
	let sum_types = Parse.parse_file filename in
	code_gen sum_types
	
	(* TODO: Think about detecting versioning mismatch between generated and ocaml ast.*)
	(* Maybe keep a version number around someplace?*)


and code_gen (sum_types : Parse.sum_type list) =
	let cg = Codegen.make_codegen_with_channel stdout in
	List.iter (gen_code_for_sum_type cg) sum_types
	
and gen_code_for_sum_type (cg : Codegen.t) (sum_type : Parse.sum_type) =
	begin
		if sum_type_is_enum sum_type then
			emit_constant_sum_type cg sum_type
		else
			(* TODO: Implement me! *)
			Log.unimp "Non-enum type in process_sum_type"
	end ;
	Codegen.flush cg
	

and sum_type_is_enum (sum_type : Parse.sum_type) : bool =
	let (_, branches) = sum_type in
	List.for_all (fun branch -> 
			let (_, components) = branch in
	 		List.length components == 0
		) branches
	
	

and enum_interface_for_constant_sum_type (sum_type : Parse.sum_type) : Codegen.enum_interface =
	let (sum_type_name, branches) = sum_type in
	
	(* Explicitly make the first enum have value 0, the second have value 2, etc.*)
	let branch_to_element i branch =
		let (branch_name, _) = branch in
		 (branch_name, (Some i))
	in
	let enum_elements = List.mapi branch_to_element branches in
	{Codegen.enum_name = sum_type_name;
	 Codegen.enum_elements = enum_elements
	}

	
and emit_constant_sum_type (cg : Codegen.t) (sum : Parse.sum_type) : unit =
	let enum_interface = enum_interface_for_constant_sum_type sum in
	Codegen.emit_enum_interface cg enum_interface

	
let () =
	parse_and_generate "hello_ast.ml"
	