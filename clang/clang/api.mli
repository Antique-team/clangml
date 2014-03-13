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
  | TranslationUnit : Ast.decl request

  (* Get the main unit filename. *)
  | Filename : string request

  (* Get the canonical type for a ctyp node. *)
  | CanonicalType : Ast.ctyp Ref.t -> Ast.ctyp request

  (* Get the type for a type_loc. *)
  | TypePtr : Ast.tloc Ref.t -> Ast.ctyp request

  (* Get the presumed location for a source location. *)
  | PresumedLoc : Sloc.t -> Sloc.presumed_loc request

  (* Returns true if the file of provided SourceLocation is
     the main file. *)
  | IsFromMainFile : Sloc.t -> bool request


type error =
  | E_Unhandled of string
  | E_NullRef
  | E_Version of string

val string_of_error : error -> string

(* Server *)
type 'a response =
  | Error of error
  | Success of 'a

exception E of error

val failure : error -> 'a
val connect : ('a request -> 'a) -> unit

(* Client *)
type clang

val request : clang -> 'a request -> 'a
val parse : string list -> (clang -> 'a) -> 'a
