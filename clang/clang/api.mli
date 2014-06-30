(* C++ side context. It is required on the server side
   to resolve 'a Ref.t to actual clang AST node objects.
   This context object is never sent to the client. *)
type context


(* Identifier to select the cache to be returned in the
   CacheFor request. *)
type _ cache_type =
  | Cache_ctyp : Ast.ctyp cache_type


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

  (* Get the canonical type for a ctyp node. A canonical type
     is the type with any typedef names stripped out of it or
     the types it references. *)
  | CanonicalType : Ast.ctyp Ref.t -> Ast.ctyp request
  (* Returns the size of type source info data block for the given type. *)
  | SizeofType : Ast.ctyp Ref.t -> int64 request
  (* Returns the alignment of type source info data block for
     the given type. *)
  | AlignofType : Ast.ctyp Ref.t -> int request

  (* Get the type for a type_loc. *)
  | TypePtr : Ast.tloc Ref.t -> Ast.ctyp request

  (* Returns the "presumed" location of a SourceLocation specifies.

     A "presumed location" can be modified by [#line] or GNU line marker
     directives.  This provides a view on the data that a user should see
     in diagnostics, for example.
 
     Note that a presumed location is always given as the expansion point of
     an expansion location, not at the spelling location.
 
     Returns the presumed location of the specified SourceLocation. If the
     presumed location cannot be calculate (e.g., because [loc] is invalid
     or the file containing [loc] has changed on disk), returns an invalid
     presumed location. *)
  | PresumedLoc : Sloc.t -> Sloc.presumed_loc request

  (* Returns true if the file of provided SourceLocation is
     the main file. *)
  | IsFromMainFile : Sloc.t -> bool request

  (* Return the file characteristic of the specified source
     location, indicating whether this is a normal file, a system
     header, or an "implicit extern C" system header.

     This state can be modified with flags on GNU linemarker directives like:
     [# 4 "foo.h" 3]
     which changes all source locations in the current file after that to be
     considered to be from a system header. *)
  | FileCharacteristic : Sloc.t -> Sloc.characteristic_kind request

  (* Return the internal cache for an OCaml type. This is an array containing
     every value of that type returned by the API, including recursively
     reachable values. *)
  | CacheFor : 'a cache_type -> 'a Util.DenseIntMap.t request


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
