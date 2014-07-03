open Clang
open Ast

external clang_presumed_loc		: Api.context -> Sloc.t -> Sloc.presumed_loc = "clang_presumed_loc"
external clang_is_from_main_file	: Api.context -> Sloc.t -> bool = "clang_is_from_main_file"
external clang_characteristic_kind	: Api.context -> Sloc.t -> Sloc.characteristic_kind = "clang_characteristic_kind"
external clang_type_sizeof		: Api.context -> ctyp Ref.t -> int64 = "clang_type_sizeof"
external clang_type_alignof		: Api.context -> ctyp Ref.t -> int = "clang_type_alignof"

external clang_cache_for_ctyp		: Api.context -> ctyp Util.DenseIntMap.t = "clang_cache_for_ctyp"


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

    | SizeofType ty ->
        clang_type_sizeof ctx ty
    | AlignofType ty ->
        clang_type_alignof ctx ty

    | PresumedLoc sloc ->
        if Sloc.is_valid sloc then
          clang_presumed_loc ctx sloc
        else
          Sloc.invalid_presumed

    | IsFromMainFile sloc ->
        clang_is_from_main_file ctx sloc

    | FileCharacteristic sloc ->
        clang_characteristic_kind ctx sloc

    | CacheFor Cache_ctyp ->
        let types = clang_cache_for_ctyp ctx in
        Invariants.check_type_map types;
        types
  in

  Gc.compact ();
  Api.connect handle_request


external check_ast_bridge_version : string -> unit = "check_ast_bridge_version"

let () =
  (* Check that Ast and AstBridge are the same version. *)
  assert (Ast.version = AstBridge.version);
  (* Check C++ side of bridge. *)
  check_ast_bridge_version AstBridge.version;
  Callback.register "success" run_processor;
  Callback.register "failure" failwith;
;;
