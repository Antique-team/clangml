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


let failure e =
  raise (E e)


(* Server function. *)
let respond input output (handle : 'a request -> 'a) : unit =
  let request = Marshal.from_channel input in
  let response =
    try
      Success (handle request)
    with E error ->
      Error error
  in
  Marshal.to_channel output response [];
  flush output


(* Client function. *)
let request (msg : 'a request) : 'a =
  Marshal.to_channel stdout msg [];
  flush stdout;
  match Marshal.from_channel stdin with
  | Error error ->
      raise (E error)
  | Success value ->
      value
