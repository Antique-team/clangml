open Clang
open Ast

external clang_canonical_type	: Api.context -> ctyp Ref.t -> ctyp = "clang_canonical_type"
external clang_type_ptr		: Api.context -> tloc Ref.t -> ctyp = "clang_type_ptr"
external clang_presumed_loc	: Api.context -> Sloc.t -> Sloc.presumed_loc = "clang_presumed_loc"


let run_processor (tu : decl) (file : string) (ctx : Api.context) =
  let open Api in

  let rec handle_request : type a. a request -> a = function
    | Compose (msg1, msg2) ->
        let res1 = handle_request msg1 in
        let res2 = handle_request msg2 in
        res1, res2

    | Handshake version ->
        if version = Ast.version then
          Ast.version
        else
          failure @@ E_Version Ast.version

    | TranslationUnit ->
        tu

    | Filename ->
        file

    | CanonicalType id ->
        clang_canonical_type ctx id

    | TypePtr tloc ->
        if Ref.is_null tloc then
          failure E_NullRef
        else
          clang_type_ptr ctx tloc

    | PresumedLoc sloc ->
        if Sloc.is_valid sloc then
          clang_presumed_loc ctx sloc
        else
          Sloc.invalid_presumed
  in

  Gc.compact ();
  Api.connect handle_request


external check_bridge_version : string -> unit = "check_bridge_version"

let () =
  (* Check that Ast and Bridge are the same version. *)
  assert (Ast.version = Bridge.version);
  (* Check C++ side of bridge. *)
  check_bridge_version Bridge.version;
  Callback.register "success" run_processor;
  Callback.register "failure" failwith;
;;
