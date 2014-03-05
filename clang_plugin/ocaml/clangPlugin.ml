open ClangBridge

external check_bridge_version : string -> unit = "check_bridge_version"

let null = Format.formatter_of_out_channel (open_out "/dev/null")
(*let null = Format.formatter_of_out_channel stdout*)


let run_processor send pp name value file =
  Gc.compact ();
  let proc = Unix.open_process_out "../ast-processor/main.byte" in

  Marshal.to_channel proc (ClangApi.List [
      ClangApi.Filename file;
      ClangApi.AstNode (send value);
    ]) [];

  match Unix.close_process_out proc with
  | Unix.WEXITED 0 -> (* all went fine *) ()
  | Unix.WEXITED   status -> failwith ("WEXITED "   ^ string_of_int status)
  | Unix.WSIGNALED status -> failwith ("WSIGNALED " ^ string_of_int status)
  | Unix.WSTOPPED  status -> failwith ("WSTOPPED "  ^ string_of_int status)


let print_expr : expr -> string -> unit =
  run_processor (fun x -> ClangApi.Expr x) ClangPp.pp_expr "Expression"

let print_stmt : stmt -> string -> unit =
  run_processor (fun x -> ClangApi.Stmt x) ClangPp.pp_stmt "Statement"

let print_decl : decl -> string -> unit =
  run_processor (fun x -> ClangApi.Decl x) ClangPp.pp_decl "Declaration"


let () =
  check_bridge_version ClangBridge.version;
  (*TODO: standardize convention for naming*)
  Callback.register "Hello print expr" print_expr;
  Callback.register "Hello print stmt" print_stmt;
  Callback.register "Hello print decl" print_decl;
  Callback.register "Hello failure" failwith;
;;
