open Ast
open Prelude

(* C++ side context. It is required on the server side
   to resolve 'a Ref.t to actual clang AST node objects.
   This context object is never sent to the client. *)
type context


(* Request messages use a GADT to enable type-safe communication.
   Messages are designed to be compact, so most clang query
   messages will operate on 'a Ref.t references. A wrapper
   function may extract these from the respective 'a. *)
type _ request =
  (* Handshake:
     - client sends its AST version
     - server checks version and sends a communication token
       in case they are equal, and raises E_Version otherwise.
     We keep the handshake constructor in the first place,
     so its binary interface remains stable. *)
  | Handshake : (* version *)string -> string request

  (* Compose two messages. This composition can nest arbitrarily,
     enabling a user to create any command tree. The server
     processes this tree depth-first left-to-right. *)
  | Compose : 'a request * 'b request -> ('a * 'b) request

  (* Get the TranslationUnit decl node. This node is immutable
     and always available on the server. This function is used
     to get the initial TU node. *)
  | TranslationUnit : decl request

  (* Get the main unit filename. *)
  | Filename : string request

  (* Get the canonical type for a ctyp node. *)
  | CanonicalType : ctyp Ref.t -> ctyp request

  (* Get the type for a type_loc. *)
  | TypePtr : tloc Ref.t -> ctyp request


type error =
  | E_Unhandled of string
  | E_NullRef
  | E_Version of string

type 'a response =
  | Error of error
  | Success of 'a

exception E of error


(* Server functions. *)
let connect (handle : 'a request -> 'a) =
  let (server_read, server_write) =
    Marshal.from_string (Scanf.unescaped @@ Sys.getenv "PIPE_FDS") 0
  in

  let input  = Unix. in_channel_of_descr server_read  in
  let output = Unix.out_channel_of_descr server_write in

  let rec io_loop () =
    (* Read request. *)
    let request = Marshal.from_channel input in

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
    io_loop ()
  with End_of_file ->
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
  Marshal.to_channel output msg [];
  flush output;
  match Marshal.from_channel input with
  | Error error ->
      raise (E error)
  | Success value ->
      value


let parse args continue =
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

      let argv =
        Array.of_list @@
        [
          "env";
          "clang";
          "-fsyntax-only";
          "-Xclang"; "-load";
          "-Xclang"; "_build/clangaml.dylib";
          "-Xclang"; "-analyze";
          "-Xclang"; "-analyzer-checker=external.OCaml";
        ] @ args
      in

      let env =
        (* The fd numbers are passed in the environment. In the
           client process, these fds are still open, but the program
           needs to be told what they are. *)
        [|"PIPE_FDS=" ^ pipe_fds|]
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
             failwith @@ "sub-process " ^ msg ^ " " ^ string_of_int status
           in

           (* Wait for child process to end and retrieve process status. *)
           match snd (Unix.waitpid [] pid) with
           | Unix.WEXITED 0 ->
               (* all went fine *) ()
           | Unix.WEXITED   status -> fail "exited with status" status
           | Unix.WSIGNALED status -> fail "was killed by signal" status
           | Unix.WSTOPPED  status -> fail "was stopped by signal" status
        )
