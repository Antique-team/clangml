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
  | E_Unhandled of string
  | E_NullRef
  | E_Version of string

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
