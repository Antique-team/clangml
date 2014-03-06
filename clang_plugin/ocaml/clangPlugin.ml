open ClangAst

external clang_canonical_type : ClangApi.context -> ctyp Clang.t -> ctyp = "clang_canonical_type"


let run_processor (tu : decl) (file : string) (ctx : ClangApi.context) =
  Gc.compact ();
  let (input, output) = Unix.open_process "../ast-processor/main.byte" in

  let rec handle_request : type a. a ClangApi.request -> a = let open ClangApi in function
    | Compose (msg1, msg2) ->
        handle_request msg1, handle_request msg2

    | Handshake version ->
        if version = ClangAst.version then
          None
        else
          Some ClangAst.version

    | TranslationUnit ->
        tu

    | Filename ->
        file

    | CanonicalType id ->
        clang_canonical_type ctx id
  in

  let rec io_loop () =
    ClangApi.respond input output handle_request;

    io_loop ()
  in

  try
    io_loop ()
  with End_of_file ->
    match Unix.close_process (input, output) with
    | Unix.WEXITED 0 -> (* all went fine *) ()
    | Unix.WEXITED   status -> failwith ("WEXITED "   ^ string_of_int status)
    | Unix.WSIGNALED status -> failwith ("WSIGNALED " ^ string_of_int status)
    | Unix.WSTOPPED  status -> failwith ("WSTOPPED "  ^ string_of_int status)


external check_bridge_version : string -> unit = "check_bridge_version"

let () =
  (* Check that Ast and Bridge are the same version. *)
  assert (ClangAst.version = ClangBridge.version);
  (* Check C++ side of bridge. *)
  check_bridge_version ClangBridge.version;
  Callback.register "success" run_processor;
  Callback.register "failure" failwith;
;;
