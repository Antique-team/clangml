(* Server context *)
type context

(* Common communication types *)
type _ request =
  | Handshake : string -> string request
  | Compose : 'a request * 'b request -> ('a * 'b) request
  | TranslationUnit : Ast.decl request
  | Filename : string request
  | CanonicalType : Ast.ctyp Ref.t -> Ast.ctyp request
  | TypePtr : Ast.tloc Ref.t -> Ast.ctyp request

type error =
  | E_NullRef
  | E_Version of string

(* Server *)
type 'a response =
  | Error of error
  | Success of 'a

exception E of error

val failure : error -> 'a
val spawn : string -> ('a request -> 'a) -> Unix.process_status

(* Client *)
type clang

val request : clang -> 'a request -> 'a
val connect : (clang -> 'a) -> unit
