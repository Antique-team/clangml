open ClangAst

(* Opaque C++ side context. This type is not serialisable. *)
type context


(* Request messages use a GADT to enable type-safe communication. *)
type _ request =
  (* Handshake:
     - client sends its AST version
     - server checks version and sends None in case they are
       equal, Some version with its own version otherwise.
     We keep the handshake constructor in the first place,
     so its binary interface remains stable. *)
  | Handshake : (* version *)string -> string option request
  (* Compose several messages. *)
  | Compose : 'a request * 'b request -> ('a * 'b) request
  (* Get the TranslationUnit decl node. *)
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
