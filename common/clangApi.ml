(* Messages prefixed with R_ are requests, the ones prefixed
   with S_ are responses.

   We use the node types from ClangBridge instead of ClangAst,
   so we don't pull in deriving. *)
type request =
  (* Handshake:
     - client sends its AST version
     - server checks version and sends None in case they are
       equal, Some version with its own version otherwise.
     We keep the handshake constructor in the first place,
     so its binary interface remains stable. *)
  | R_Handshake of (* version *)string
  (* Compose several messages. *)
  | R_List of request list
  (* Get the TranslationUnit decl node. *)
  | R_TranslationUnit
  (* Get the main unit filename. *)
  | R_Filename


type response =
  | S_Handshake of (* version *)string option
  | S_List of response list
  | S_TranslationUnit of ClangBridge.decl
  | S_Filename of string


(* Server functions. *)
let recv input : request =
  Marshal.from_channel input

let send output (msg : response) : unit =
  Marshal.to_channel output msg [];
  flush output


(* Client functions. *)
let request (msg : request) : response =
  Marshal.to_channel stdout msg [];
  flush stdout;
  Marshal.from_channel stdin
