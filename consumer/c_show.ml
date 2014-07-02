(** MemCAD analyzer
 ** 
 ** c_sig.ml
 ** a micro C frontend
 ** Xavier Rival, 2011/07/09 *)
open Data_structures
open Lib

(** A reduced C subset, without control flow inside expressions
 **   . function calls and procedure calls as statements
 **   . assignments as statements
 **   . no more non assignment, non function call expression statement *)

(** Improvements to consider:
 ** - localisation should be done with a notion of extent ?
 ** - simplify the language as much as possible (clean subset of C) *)

(** Localisation *)
(* Localisation information is just a line number now
 * (depending on context, first or last line of statement) *)
type c_loc = int
  deriving (Show)

(** Types *)
type c_type = C_sig.c_type =
  | Ctint
  | Ctchar
  | Ctvoid
  | Ctnamed of c_named
  | Ctstruct of c_aggregate
  | Ctunion  of c_aggregate
  | Ctptr of c_type option (* None when what is pointed to is unknown *)
  | Ctarray of c_type * int
and c_aggregate = C_sig.c_aggregate =
    { cag_name:   string option;   (* name of the aggregate, if any *)
      cag_align:  int;
      cag_size:   int;
      cag_fields: c_agg_field list (* fields of the aggregate *) }
and c_agg_field = C_sig.c_agg_field = (* field aggregate (used for unions and structures) *)
    { caf_typ:  c_type;
      caf_off:  int;
      caf_size: int;
      caf_name: string }
and c_named = C_sig.c_named =
    { cnt_name:         string (* name of the type *) ;
      mutable cnt_type: c_type (* type pointed *) }
  deriving (Show)
(** Operators *)
type c_uniop = C_sig.c_uniop =
  | Cuneg
  deriving (Show)
type c_binop = C_sig.c_binop =
  | Cbeq
  | Cbne
  | Cbge | Cbgt | Cble | Cblt
  | Cbadd | Cbsub | Cbmul
  | Cbland | Cblor
  deriving (Show)
(** Variables *)
type c_var = C_sig.c_var =
    { cv_name:     string (* name *) ;
      cv_uid:      int    (* unique ID *) ;
      cv_type:     c_type (* type *) ;
      cv_volatile: bool   (* whether it is volatile *) }
  deriving (Show)
(** Constants *)
type c_const = C_sig.c_const =
  | Ccint of int
  | Ccnull       (* null pointer, for the sake of typing *)
  deriving (Show)
(** Expressions *)
type c_exprk = C_sig.c_exprk =
  | Ceconst of c_const
  | Celval of c_lval
  | Ceaddrof of c_lval
  | Ceuni of c_uniop * c_expr
  | Cebin of c_binop * c_expr * c_expr
and c_expr = C_sig.c_expr =
    { cek: c_exprk (* the actual expression *) ;
      cet: c_type  (* type *) }
and c_lvalk = C_sig.c_lvalk =
  | Clvar of c_var
  | Clfield of c_lval * string
  | Clindex of c_lval * c_expr
  | Clderef of c_expr
and c_lval = C_sig.c_lval =
    { clk: c_lvalk (* the actual l-value *) ;
      clt: c_type  (* type *) }
  deriving (Show)
(** MemCAD commands (share with dom_sig stuff ?) *)
type c_memcad_iparam = C_sig.c_memcad_iparam =
  | Cmp_const of int
  | Cmp_lval of c_lval
  deriving (Show)
type c_memcad_iparams = C_sig.c_memcad_iparams =
    { mc_pptr:  c_memcad_iparam list ;
      mc_pint:  c_memcad_iparam list ;
      mc_pset:  c_memcad_iparam list }
  deriving (Show)
type c_memcad_com = C_sig.c_memcad_com =
  (** asserting/verifying inductives and associated assumptions *)
  (* assumption *)
  | Mc_add_inductive of c_lval * string option * c_memcad_iparams option
  (* a semantic equivalent to assert *)
  | Mc_check_inductive of c_lval * string option * c_memcad_iparams option
  (* checks the current flow is (not) _|_ (non-_|_ not sound, for testing) *)
  | Mc_check_bottomness of bool
  (** manual management of unfolding *)
  (* provokes unfolding of an inductive *)
  | Mc_unfold of c_lval
  (* provokes backward unfolding of a segment towards some node *)
  | Mc_unfold_bseg of c_lval
  (** disjunctions management *)
  (* unrolling of a loop, over-riding the main parameter *)
  | Mc_unroll of int
  (* provokes merging of all disjuncts (if using the disjunctive domain) *)
  | Mc_merge
  (** output and analysis manual control *)
  (* output of a graph *)
  | Mc_output
  (* forces a variable be considered live *)
  | Mc_force_live of c_var list
  (* kill current flow into _|_ *)
  | Mc_kill_flow
  (** reduction *)
  (* provokes the re-localization of a node, after reduction *)
  | Mc_reduce_localize of c_lval
  (* eager reduction *)
  | Mc_reduce_eager
  (** unparsed *)
  (* status of the string while yet unparsed *)
  | Mc_comstring of string
  deriving (Show)
(** Calls *)
type c_call = C_sig.c_call =
    { cc_fun:  c_expr ;
      cc_args: c_expr list }
  deriving (Show)
(** Statements *)
type c_statk = C_sig.c_statk =
  (* variables declaration *)
  | Csdecl of c_decl
  (* procedure calls and function calls *)
  | Cspcall of c_call (* procedure, does not have a return value *)
  | Csfcall of c_lval * c_call (* function, returns a value *)
  (* assignment statement *)
  | Csassign of c_lval * c_expr
  (* block *)
  | Csblock of c_block
  (* condition *)
  | Csif of c_expr * c_block * c_block
  (* while loop *)
  | Cswhile of c_expr * c_block * int option (* number of unrolls *)
  (* jump statements *)
  | Csreturn of c_expr option
  | Csbreak
  (* parameterization of the analysis *)
  | Cs_memcad of c_memcad_com (* MemCAD command *)
  | Csassert of c_expr (* conventional C assert *)
  (* memory management *)
  | Csalloc of c_lval * c_expr (* allocation of a number of bytes *)
  | Csfree of c_lval
and c_stat = C_sig.c_stat =
    { csk: c_statk (* the actual statement *) ;
      csl: c_loc   (* localization information; line number for now *) }
and c_block = c_stat list
and c_decl = c_var
  deriving (Show)
type c_fun = C_sig.c_fun =
    { cf_type: c_type ;
      cf_uid:  int ; (* notion of uid for functions *)
      cf_name: string ;
      cf_args: c_var list ;
      cf_body: c_block }
  deriving (Show)
type c_prog = C_sig.c_prog =
    { cp_vars:  c_var StringMap.t ;
      cp_funs:  c_fun StringMap.t ;
      cp_types: c_type StringMap.t }
