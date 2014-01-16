open Hello_ast


let print_expr (e: expr) : unit =
  Gc.compact ();
  Format.printf "@[<v2>Statement is:@,%a@]@." Hello_pp.pp_expr e


let () =
  Callback.register "Hello print expr" print_expr; (*TODO: standardize convention for naming*)
