open ClangAst


let run_processor (tu : decl) (file : string) (ctx : ClangApi.context) =
  Gc.compact ();
  let (input, output) = Unix.open_process "../ast-processor/main.byte" in

  let rec respond = let open ClangApi in function
    | R_List msgs ->
        S_List (List.map respond msgs)

    | R_Handshake version ->
        if version = ClangAst.version then
          S_Handshake None
        else
          S_Handshake (Some ClangAst.version)

    | R_TranslationUnit ->
        S_TranslationUnit tu

    | R_Filename ->
        S_Filename file
  in

  let rec io_loop () =
    ClangApi.send output (
      let request = ClangApi.recv input in
      respond request
    );

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
