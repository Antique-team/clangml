open Hello_ast

external check_bridge_version : string -> unit = "check_bridge_version"


let print_expr (e: expr) : unit =
  Gc.compact ();
  Format.printf "@[<v2>Expression is:@,%a@]@." Hello_pp.pp_expr e

let print_stmt (s: stmt) : unit =
  Gc.compact ();
  Format.printf "@[<v2>Statement is:@,%a@]@." Hello_pp.pp_stmt s

let print_decl (d: decl) : unit =
  Gc.compact ();
  Format.printf "@[<v2>Declaration is:@,%a@]@." Hello_pp.pp_decl d


let () =
  check_bridge_version Hello_ast.version;
  (*TODO: standardize convention for naming*)
  Callback.register "Hello print expr" print_expr;
  Callback.register "Hello print stmt" print_stmt;
  Callback.register "Hello print decl" print_decl;
;;
