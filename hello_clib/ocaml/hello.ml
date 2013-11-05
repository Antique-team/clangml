open Hello_ast

external print_hello: unit -> unit = "caml_print_hello"

    
let () =
  print_hello ()


let print_hello_in_ocaml () =
  print_endline "Hello World from ocaml"


  

let () =
  print_endline "About to register";
  Callback.register "Hello callback" print_hello_in_ocaml;







external create_stmt: unit -> stmt = "create_stmt"


type value =
	| UnitVal
	| IntVal of int


exception Eval_error


let apply_op op v1 v2 =
	match (v1, v2) with
		| (IntVal(i1), IntVal(i2)) -> begin
			match op with
			| BinaryOp_Add -> IntVal(i1 + i2)
			| BinaryOp_Multiply -> IntVal(i1 * i2)
		end
		| (_, _) -> raise Eval_error


let print_val v = 
	match v with
	| IntVal(i) -> print_endline (string_of_int i)
	| _ -> raise Eval_error

let rec eval_expr (e : expr) : value =
	match e with
	| Unit -> UnitVal
	| IntConst i -> IntVal i
	| BinaryOp(op, e1, e2) -> 
		let v1 = (eval_expr e1) in
		let v2 = (eval_expr e2) in
		apply_op op v1 v2
	| ConstDecl (name, e1, e2) -> raise Eval_error (*TODO:*)
	| Var name -> raise Eval_error (*TODO:*)

and eval_stmt (s : stmt) : unit =
	match s with
	| Skip -> ()
	| Print(e) -> print_val (eval_expr e)
	| Block ss -> List.iter eval_stmt ss



let () = 
	let s = create_stmt () in
	print_endline "Created statement";
	Format.printf "@[<v 2>Statement is:@,%a@]@." Hello_pp.pp_stmt s
	(*
	let v1 = IntConst 2 in
	let e1 = Var foo in
	let s = ConstDecl foo v1 e1 in
	Format.printf "@[<v 2>Statement is:@,%a@]@." Hello_pp.pp_stmt s;
	*)

let () =
	let v1 = ConstDecl ("foo", IntConst 2, BinaryOp(BinaryOp_Add, Var "foo", IntConst 2)) in
	print_endline "Created statement 2";
	Format.printf "@[<v 2>Statement is:@,%a@]@." Hello_pp.pp_expr v1
	
    
