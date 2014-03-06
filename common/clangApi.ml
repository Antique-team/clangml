open ClangAst

(* C++ side context. It is required on the server side
   to resolve 'a Clang.t to actual clang AST node objects.
   This context object is never sent to the client. *)
type context


(* Request messages use a GADT to enable type-safe communication.
   Messages are designed to be compact, so most clang query
   messages will operate on 'a Clang.t references. A wrapper
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
  | CanonicalType : ctyp Clang.t -> ctyp request


(* Server function. *)
let respond input output (handle : 'a request -> 'a) : unit =
  let request = Marshal.from_channel input in
  Marshal.to_channel output (handle request) [];
  flush output


(* Client function. *)
let request (msg : 'a request) : 'a =
  Marshal.to_channel stdout msg [];
  flush stdout;
  Marshal.from_channel stdin
