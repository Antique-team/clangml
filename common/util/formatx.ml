(** Extend the [Format] standard library module. *)    
include Format
    
(** A string separator followed by a breaking space. *)
let pp_sep_space (sep: string) fmt () = fprintf fmt "%s@ " sep

(** A comma separator with a space. *)
let pp_comma_space = pp_sep_space ","

(** A colon separator with a space. *)
let pp_colon_space = pp_sep_space ":"

(** A semi-colon separator with a space. *)
let pp_semicolon_space = pp_sep_space ";"

(** A string separator followed by a break. *)
let pp_sep (sep: string) fmt () = fprintf fmt "%s@," sep

(** A comma separator. *)
let pp_comma_sep = pp_sep ","

(** A hard space separator. For use when we can't use '@ '  because it is never appropriate to turn space into a newline. *)
let pp_hard_space fmt () = fprintf fmt " "

(** A block with open/close strings. *)
let pp_align_block (open_symbol: string) (close_symbol: string) (pp_body: formatter -> unit -> unit) fmt () =
  fprintf fmt "%s@[%a@]%s" open_symbol pp_body () close_symbol
  
(** Pretty-print pairs. *)
let pp_2tuple ?sep:(sep = pp_comma_sep) (pp_a: formatter -> 'a -> unit) (pp_b: formatter -> 'b -> unit) fmt (a,b) =
  pp_align_block "(" ")" (fun fmt () -> fprintf fmt "@[%a@]%a@[%a@]" pp_a a sep () pp_b b) fmt ()

(** Pretty-print using an iterator. *)
let pp_iterable
      ?sep:(sep = pp_comma_space)
      (iter: ('e -> unit) -> 'a -> unit)
      (pp_element: formatter -> 'e -> unit)
      fmt
      (obj: 'a) =
  let first = ref true in
  fprintf fmt "%t"
    (fun fmt ->
      iter
        (fun x -> if !first then (first := false; fprintf fmt "@[%a@]" pp_element x)
                  else fprintf fmt "%a@[%a@]" sep () pp_element x)
         obj)
        
(** Pretty-print a list. *)        
let pp_list ?sep:(sep = pp_comma_space) pp_element fmt (l: 'a list) =
  pp_iterable ~sep:sep List.iter pp_element fmt l
  
(** Create a basic maplet pretty-printer from key and value printers. *)  
let pp_maplet
      ?sep:(sep = pp_colon_space)
      (pp_key: formatter -> 'k -> unit)
      (pp_value: formatter -> 'v -> unit)
      fmt
      ((k,v): 'k * 'v) =
  fprintf fmt "@[<hv 2>@[%a@]%a@[%a@]@]" pp_key k sep () pp_value v
        
(** Extend a [Map.S] with pretty-printing. *)
module Mapx (M: Map.S) = struct
  include M
  
  (** An [iter] that takes an uncurried iterator. *)
  let iteru (f: key * 'v -> unit) (map: 'v t) =
    iter (fun k v -> f (k, v)) map
  
  (** Pretty-print a map. *)
  let pp_t
        ?sep:(sep = pp_comma_space)
        (pp_maplet: formatter -> (key * 'v) -> unit)
        fmt
        (map: 'v t) =
    pp_iterable ~sep:sep iteru pp_maplet fmt map        
end

(** Extend a [Set.S] with pretty-printing. *)
module Setx (S: Set.S) = struct
  include S
  
  (** Pretty-print a set. *)
  let pp_t
        ?sep:(sep = pp_comma_space)
        (pp_elt: formatter -> elt -> unit)
        fmt
        (set: t) =
    pp_iterable ~sep:sep S.iter pp_elt fmt set
end

(** {2 Convert between pretty-printers and [to_string] functions} *)  
  
(** Create a pretty-printer from a [to_string] function. *)  
let pp_from_to_string (to_string: 'a -> string) fmt (x: 'a) =
  fprintf fmt "%s" (to_string x)
  
(** A local formatter and flushing function used by {!pp_to_string}. *)
let (pp_to_string_formatter: formatter), (flush_pp_to_string_formatter: unit -> string) =
  let buffer = Buffer.create 64 in
  let formatter = formatter_of_buffer buffer in
  let flusher () =
    pp_print_flush formatter (); let str = Buffer.contents buffer in Buffer.clear buffer; str
  in
  formatter, flusher
  
(** Make a pretty-printer into a [to_string] function. *)  
let pp_to_string (pp: formatter -> 'a -> unit) (x: 'a) : string =
  pp pp_to_string_formatter x; flush_pp_to_string_formatter ()
 