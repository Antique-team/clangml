external print_hello: unit -> unit = "caml_print_hello"

let () =
  print_hello ()


let print_hello_in_ocaml () =
  print_endline "Hello World from ocaml"


let () =
  Callback.register "Hello callback" print_hello_in_ocaml;


type binary_op =
	| BinaryOp_Add
	| BinaryOp_Multiply


type expr =
	| Val of int
	| BinaryOp of binary_op * expr * expr



let apply_op op v1 v2 =
	match op with
	| BinaryOp_Add -> v1 + v2
	| BinaryOp_Multiply -> v2 * v2



let rec eval (e : expr) : int =
	match e with
	| Val i -> i
	| BinaryOp(op, e1, e2) -> 
		let v1 = (eval e1) in
	let v2 = (eval e2) in
		apply_op op v1 v2

