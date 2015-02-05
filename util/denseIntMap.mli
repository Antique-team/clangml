(* A typed key. The ghost type helps retain type safety and prevents
   direct indexing in the typed map. If direct indexing is required,
   the map itself can be coerced to an array. *)
type 'key key = private int

val null_key : 'key key

module Show_key :
  functor (S : Deriving_Show.Show) ->
    Deriving_Show.Show
      with type a = S.a key


type ('key, 'value) t = private 'value array


val find : 'key key -> ('key, 'value) t -> 'value
  (* Retrieve a value for a key. *)
val to_array : ('key, 'value) t -> 'value array
  (* Retrieve the array of values. *)
val iter : ('key key -> 'value -> unit) -> ('key, 'value) t -> unit
  (* Iterate over keys and their respective values. *)
val fold : ('key key -> 'value -> 'a -> 'a) -> ('key, 'value) t -> 'a -> 'a
  (* Fold over the map as in Map.S.fold. *)
val map
  :  ('key_a key -> 'value_a -> 'value_b)
  -> ('key_a, 'value_a) t
  -> 'key_b key array * ('key_b, 'value_b) t
  (* [map f m] creates a new map of the same cardinality and maps the 'a keys to
     'b keys with the same index. The keys are returned as an additional array. *)
val mapv : ('key key -> 'a -> 'b) -> ('key, 'a) t -> ('key, 'b) t
  (* Map values, keep the same keys. *)
val mapvf : ('key key -> 'a -> 'state -> 'state * 'b) -> ('key, 'a) t -> 'state -> 'state * ('key, 'b) t
  (* Map values, keep the same keys, also fold over a state. *)

(*val mapi : ('a key -> 'a -> int * 'b) -> 'a t -> 'b t*) (* TODO *)
  (* [map f m] creates a new map of the same cardinality, but changes the order
     of the mappings according to the first return value of [f]. This number
     must be lower than the cardinality of the map. *)

val cardinal : ('key, 'value) t -> int
val exists : ('value -> bool) -> ('key, 'value) t -> bool
