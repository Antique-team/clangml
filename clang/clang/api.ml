open Ast
open Util.Prelude

(* Server context *)
type context

type _ cache_type =
  | Cache_ctyp : Ast.ctyp cache_type


(* Common communication types *)
type _ request =
  | Handshake : string -> string request
  | Compose : 'a request * 'b request -> ('a * 'b) request
  | TranslationUnit : Ast.decl request
  | Filename : string request
  | SizeofType : Ast.ctyp Ref.t -> int64 request
  | AlignofType : Ast.ctyp Ref.t -> int request
  | DeclOfType : Ast.ctyp Ref.t -> Ast.decl request
  | PresumedLoc : Sloc.t -> Sloc.presumed_loc request
  | IsFromMainFile : Sloc.t -> bool request
  | FileCharacteristic : Sloc.t -> Sloc.characteristic_kind request
  | CacheFor : 'a cache_type -> ('a, 'a) Util.DenseIntMap.t request

type error =
  | E_Unhandled of string
  | E_NullRef
  | E_Version of string
  | E_Failure of string

type 'a response =
  | Error of error
  | Success of 'a

exception E of error


let string_of_error = function
  | E_Unhandled msg -> "E_Unhandled (" ^ msg ^ ")"
  | E_NullRef -> "E_NullRef"
  | E_Version ver -> "E_Version (" ^ ver ^ ")"
  | E_Failure msg -> "E_Failure (" ^ msg ^ ")"


let name_of_request : type a. a request -> string = function
  | Handshake		_ -> "Handshake"
  | Compose		_ -> "Compose"
  | TranslationUnit	  -> "TranslationUnit"
  | Filename		  -> "Filename"
  | SizeofType		_ -> "SizeofType"
  | AlignofType		_ -> "AlignofType"
  | DeclOfType		_ -> "DeclOfType"
  | PresumedLoc		_ -> "PresumedLoc"
  | IsFromMainFile	_ -> "IsFromMainFile"
  | FileCharacteristic	_ -> "FileCharacteristic"
  | CacheFor		_ -> "CacheFor"


(* Server functions. *)
let connect (handle : 'a request -> 'a) =
  let (server_read, server_write) =
    try
      (Marshal.from_string (Scanf.unescaped @@ Sys.getenv "PIPE_FDS") 0
       : Unix.file_descr * Unix.file_descr)
    with Not_found ->
      failwith "PIPE_FDS environment variable must be set"
  in

  let input  = Unix. in_channel_of_descr server_read  in
  let output = Unix.out_channel_of_descr server_write in

  let rec io_loop () =
    (* Read request. *)
    let request = (Marshal.from_channel input: 'a request) in

    (* Handle request. *)
    let response =
      try
        Success (handle request)
      with
      | E error ->
          Error error
      | exn ->
          Error (E_Unhandled (Printexc.to_string exn))
    in

    (* Write response. *)
    Marshal.to_channel output response [];
    flush output;

    (* Next iteration. *)
    io_loop ()
  in

  try
    try
      io_loop ()
    with Failure msg ->
      begin
        if msg = "input_value: truncated object" then
          (* change of behavior when unmarshaling an empty file
             since OCaml 4.03.0 *)
          raise End_of_file (* do as in previous OCaml versions *)
        else
          raise (Failure msg)
      end
  with
  | End_of_file ->
    close_in input;
    close_out output;
;;


let failure e =
  raise (E e)


(* Client functions. *)
type clang = {
  input  : in_channel;
  output : out_channel;
  token  : string;
}

let request { input; output } (msg : 'a request) : 'a =
  Log.debug "request (%s)" (name_of_request msg);
  Marshal.to_channel output msg [];
  flush output;
  match (Marshal.from_channel input: 'a response) with
  | Error error ->
      raise (E error)
  | Success value ->
      value

(* return the full path for the command, if found in path, none else *)
let command_exists (cmd: string): string option =
  Unix.(
    if system ("which " ^ cmd ^ " 2>&1 > /dev/null") = WEXITED 0 then
      Some (List.hd (Util.Various.get_command_output ("which " ^ cmd)))
    else
      None
  )

exception Command_found of string
exception No_command_found of string

(* return the first available command from provided list of commands *)
let first_command_found (cmds: string list): string =
  try
    List.iter (fun cmd ->
        match command_exists cmd with
        | Some cmd -> raise (Command_found cmd)
        | None -> ()
      ) cmds;
    raise (No_command_found (string_of_list (fun x -> x) "; " cmds))
  with Command_found cmd -> cmd

let parse args continue =
  (* Try to find our clang plugin. *)
  let plugin =
    try
      let clangml_path =
        try
          Sys.getenv "CLANGML_PATH"
        with Not_found ->
          "."
      in
      let clangml_ocamlfind_path =
        let maybe_path =
          Util.Various.get_command_output
            "ocamlfind -query clangml"
        in match maybe_path with
        | [] -> "clangml.dylib"
        | p :: _rest -> p ^ "/clangml.dylib"
      in

      List.find (fun candidate ->
        try
          Unix.(access candidate [R_OK]);
          true
        with Unix.Unix_error _ ->
          false
      ) [
        "clangml.dylib";
        "_build/clangml.dylib";
        clangml_path ^ "/clangml.dylib";
        Config.destdir ^ "/clang/clangml.dylib";
        clangml_ocamlfind_path
      ]

    with Not_found ->
      failwith "could not find the clang ocaml plugin"
  in

  (* Communication pipe. *)
  let (client_read, server_write) = Unix.pipe () in
  let (server_read, client_write) = Unix.pipe () in

  match Unix.fork () with
  | 0 ->
      (* Close unneeded fds. *)
      List.iter Unix.close [client_read; client_write];

      (* Serialise the file descriptors. *)
      let pipe_fds =
        String.escaped (Marshal.to_string (server_read, server_write) [])
      in

      let c_compiler =
        first_command_found [
          "clang-3.9"; (* linux *)
          (Sys.getenv "HOME") ^ "/usr/clang39/bin/clang-3.9" (* osx *)
        ] in

      let argv =
        let clang = [
          (* "/usr/bin/gdb"; "--args"; *)
          c_compiler;
          "-fsyntax-only";
          "-Xclang"; "-load";
          "-Xclang"; plugin;
          "-Xclang"; "-analyze";
          "-Xclang"; "-analyzer-checker=external.OCaml";
        ] in

        Array.of_list @@ "env" :: clang @ args
      in

      let env =
        (* The fd numbers are passed in the environment. In the
           client process, these fds are still open, but the program
           needs to be told what they are. *)
        [|
          "PIPE_FDS=" ^ pipe_fds;
          "HOME=" ^ Sys.getenv "HOME";
          "TERM=" ^ Sys.getenv "TERM";
        |]
      in

      (* Start client program. *)
      begin try
        Unix.execve "/usr/bin/env" argv env
      with Unix.Unix_error (error, operation, argument) ->
        failwith @@ Format.sprintf "%s: %s %s\n"
          (Unix.error_message error) operation argument
      end

  | pid ->
      let input  = Unix. in_channel_of_descr client_read  in
      let output = Unix.out_channel_of_descr client_write in

      (* Close unneeded fds. *)
      List.iter Unix.close [server_read; server_write];

      finally
        (fun () ->
           let token =
             try
               request { input; output; token = "" } @@ Handshake Ast.version
             with E (E_Version version) ->
               failwith (
                 "AST versions do not match: server says\n"
                 ^ version ^ ", but we have\n"
                 ^ Ast.version
               )
           in

           continue { input; output; token }
        )
        (fun () ->
           close_in input;
           close_out output;

           let fail msg status =
             failwith @@ "clang sub-process " ^ msg ^ " " ^ string_of_int status
           in

           (* Wait for child process to end and retrieve process status. *)
           match snd (Unix.waitpid [] pid) with
           | Unix.WEXITED 0 ->
               (* all went fine *) ()
           | Unix.WEXITED   status -> fail "exited with status" status
           | Unix.WSIGNALED status -> fail "was killed by signal" status
           | Unix.WSTOPPED  status -> fail "was stopped by signal" status
        )
