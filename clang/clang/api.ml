open Ast

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
     - server checks version and sends None in case they are
       equal, Some version with its own version otherwise.
     We keep the handshake constructor in the first place,
     so its binary interface remains stable. *)
  | Handshake : (* version *)string -> string option request

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
  | E_NullRef

type 'a response =
  | Error of error
  | Success of 'a

exception E of error


(* Server functions. *)
let spawn prog (handle : 'a request -> 'a) : Unix.process_status =
  (* Communication pipe. *)
  let (server_read, client_write) = Unix.pipe () in
  let (client_read, server_write) = Unix.pipe () in

  match Unix.fork () with
  | 0 ->
      (* Close unneeded fds. *)
      List.iter Unix.close [server_read; server_write];

      (* Start client program. *)
      Unix.execv prog [|
        prog;
        String.escaped (Marshal.to_string (client_read, client_write) []);
      |]

  | pid ->
      let input  = Unix. in_channel_of_descr server_read  in
      let output = Unix.out_channel_of_descr server_write in

      (* Close unneeded fds. *)
      List.iter Unix.close [client_read; client_write];

      let rec io_loop () =
        (* Read request. *)
        let request = Marshal.from_channel input in

        (* Handle request. *)
        let response =
          try
            Success (handle request)
          with E error ->
            Error error
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
        (* Wait for child process to end and retrieve process status. *)
        snd (Unix.waitpid [] pid)


let failure e =
  raise (E e)


(* Client functions. *)
let request (token, input, output) (msg : 'a request) : 'a =
  Marshal.to_channel output msg [];
  flush output;
  match Marshal.from_channel input with
  | Error error ->
      raise (E error)
  | Success value ->
      value


let connect continue =
  match Sys.argv with
  | [|_; data|] ->
      let (client_read, client_write) =
        Marshal.from_string (Scanf.unescaped data) 0
      in
      let input  = Unix. in_channel_of_descr client_read  in
      let output = Unix.out_channel_of_descr client_write in

      let token =
        match request (None, input, output) @@ Handshake Ast.version with
        | None ->
            (* Handshake OK. *)
            Some Ast.version

        | Some version ->
            failwith (
              "AST versions do not match: \
               server says " ^ version ^
              ", but we have " ^ Ast.version
            )
      in

      continue (token, input, output)

  | _ ->
      failwith "NOK"
