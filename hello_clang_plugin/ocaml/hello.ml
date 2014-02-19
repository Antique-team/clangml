open Hello_ast

external check_bridge_version : string -> unit = "check_bridge_version"

let null =
  Format.formatter_of_out_channel (open_out "/dev/null")


let run_processor name arg pp value =
  Gc.compact ();
  Format.fprintf null "@[<v2>%s is:@,%a@]@."
    name pp value;
  let proc = Unix.open_process_out ("../ast-processor/main.byte " ^ arg) in

  Marshal.to_channel proc value [];

  match Unix.close_process_out proc with
  | Unix.WEXITED 0 -> (* all went fine *) ()
  | Unix.WEXITED   status -> failwith ("WEXITED "   ^ string_of_int status)
  | Unix.WSIGNALED status -> failwith ("WSIGNALED " ^ string_of_int status)
  | Unix.WSTOPPED  status -> failwith ("WSTOPPED "  ^ string_of_int status)


let print_expr (e: expr) : unit =
  run_processor "Expression" "expr" Hello_pp.pp_expr e

let print_stmt (s: stmt) : unit =
  run_processor "Statement" "stmt" Hello_pp.pp_stmt s

let print_decl (d: decl) : unit =
  run_processor "Declaration" "decl" Hello_pp.pp_decl d


let () =
  check_bridge_version Hello_ast.version;
  (*TODO: standardize convention for naming*)
  Callback.register "Hello print expr" print_expr;
  Callback.register "Hello print stmt" print_stmt;
  Callback.register "Hello print decl" print_decl;
  Callback.register "Hello failure" failwith;
;;
