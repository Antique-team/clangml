open Clang.Ast

external clang_canonical_type : Clang.Api.context -> ctyp Clang.Ref.t -> ctyp = "clang_canonical_type"
external clang_type_ptr : Clang.Api.context -> tloc Clang.Ref.t -> ctyp = "clang_type_ptr"


let run_processor (tu : decl) (file : string) (ctx : Clang.Api.context) =
  let open Clang.Api in

  let rec handle_request : type a. a request -> a = function
    | Compose (msg1, msg2) ->
        let res1 = handle_request msg1 in
        let res2 = handle_request msg2 in
        res1, res2

    | Handshake version ->
        if version = Clang.Ast.version then
          Clang.Ast.version
        else
          failure @@ E_Version Clang.Ast.version

    | TranslationUnit ->
        tu

    | Filename ->
        file

    | CanonicalType id ->
        clang_canonical_type ctx id

    | TypePtr tloc ->
        if Clang.Ref.is_null tloc then
          failure E_NullRef
        else
          clang_type_ptr ctx tloc
  in

  Gc.compact ();
  match Clang.Api.spawn "_build/consumer/processor.native" handle_request with
  | Unix.WEXITED 0 -> (* all went fine *) ()
  | Unix.WEXITED   status -> failwith ("WEXITED "   ^ string_of_int status)
  | Unix.WSIGNALED status -> failwith ("WSIGNALED " ^ string_of_int status)
  | Unix.WSTOPPED  status -> failwith ("WSTOPPED "  ^ string_of_int status)


external check_bridge_version : string -> unit = "check_bridge_version"

let () =
  (* Check that Ast and Bridge are the same version. *)
  assert (Clang.Ast.version = Clang.Bridge.version);
  (* Check C++ side of bridge. *)
  check_bridge_version Clang.Bridge.version;
  Callback.register "success" run_processor;
  Callback.register "failure" failwith;
;;