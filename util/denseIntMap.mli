(* A typed key. The ghost type helps retain type safety and prevents
   direct indexing in the typed map. If direct indexing is required,
   the map itself can be coerced to an array. *)
type 'a key = private int

val null_key : 'a key

module Show_key :
  functor (S : Deriving_Show.Show) ->
    Deriving_Show.Show
      with type a = S.a key


type 'a t = private 'a array


val find : 'a key -> 'a t -> 'a
  (* Retrieve a value for a key. *)
val iter : ('a key -> 'a -> unit) -> 'a t -> unit
  (* Iterate over keys and their respective values. *)
val fold : ('a key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (* Fold over the map as in Map.S.fold. *)
val map : ('a key -> 'a -> 'b) -> 'a t -> 'b key array * 'b t
  (* [map f m] creates a new map of the same cardinality and maps the 'a keys to
     'b keys with the same index. The keys are returned as an additional array. *)
(*val mapi : ('a key -> 'a -> int * 'b) -> 'a t -> 'b t*) (* TODO *)
  (* [map f m] creates a new map of the same cardinality, but changes the order
     of the mappings according to the first return value of [f]. This number
     must be lower than the cardinality of the map. *)
