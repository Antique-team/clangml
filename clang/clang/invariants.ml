open Ast
open Util


(* Checks whether the DenseIntMap.key invariants hold for a ctyp map. Also
   checks some other invariants for ctyp objects. *)
let check_type_map types =
  DenseIntMap.iter (fun idx ctyp ->
    (* t_self must be the map key. By deduction, this means that
       assert (DenseIntMap.find ctyp.t_self types == ctyp)
       always holds. *)
    assert (idx = ctyp.t_self);
    (* Every type T has a valid canonical type whose canonical type is
       the canonical type for T. *)
    assert ((DenseIntMap.find ctyp.t_canon types).t_canon = ctyp.t_canon);
  ) types
