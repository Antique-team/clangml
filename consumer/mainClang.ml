(** MemCAD analyzer
 **
 ** main.ml
 ** entry point, and testing
 ** Xavier Rival, 2011/05/22 *)
open Flags
open Lib
open Ast_sig
open Data_structures

(** Error report *)
let this_module = "main"
let error = gen_error this_module
let info  = gen_info  this_module
let todo  = gen_todo  this_module
let warn  = gen_warn  this_module
module T = Timer.Timer_Mod( struct let name = "Main" end )

(** Exception reporting *)
let exn_report (e: exn): unit =
  begin
    match e with
    | Apron.Manager.Error exc -> 
        Printf.printf "[FAILED IN APRON]:\n";
        flush stdout;
        Format.printf "Apron report:\n%a\n" Apron.Manager.print_exclog exc 
    | _ ->
        flush stdout;
        Printf.printf "[FAILED]: %s\n" (Printexc.to_string e)
  end;
  flush stdout;
  exit 1
          
(** Parsers *)
(* From files *)
let c_parser =
  Lib.read_from_file "c" C_parser.entry C_lexer.token
let indlist_parser =
  Lib.read_from_file "ind" Ind_parser.main_indlist Ind_lexer.token
let ilines_parser =
  Lib.read_from_file "ind" Ind_parser.main_ilines Ind_lexer.token

(** Testing parsers *)
let meta_test_parser
    (msg: string)
    (f_parse: (Lexing.lexbuf -> 'b) -> Lexing.lexbuf -> 'a)
    (f_lexe: Lexing.lexbuf -> 'b)
    (f_post: 'a -> unit)
    (files: string list): unit =
  Printf.printf "\nTest of the lexer of %s\n" msg;
  let parse_file =
    Lib.read_from_file msg f_parse f_lexe in
  List.iter
    (fun file ->
      try
        Printf.printf "parsing %s..." file;
        let x = parse_file file in
        Printf.printf "  [Ok]\n";
        f_post x
      with
      | e -> flush stdout; exn_report e
    ) files;
  Printf.printf "Finished!\n"

let run_c_parser_memcad (f: C_sig.c_prog -> unit): string list -> unit =
  meta_test_parser "c mini programs" C_parser.entry C_lexer.token
    (fun cp ->
       C_utils.ppi_c_prog "" stdout cp;
       f cp)
let run_c_parser_clang (f: C_sig.c_prog -> unit): string list -> unit =
  let (|<) x f = f x; x in
  List.iter (fun file ->
    Clang.Api.parse ["-include"; "/home/pippijn/code/git/clangaml/memcad.h";
                     "-w"; file] (fun clang ->
      let open Clang.Api in
      request clang @@ TranslationUnit
      |> Transformation.RemoveImplicitCast.transform_decl clang
      |> Transform.c_prog_from_decl clang
      |< C_utils.ppi_c_prog "" stdout
      |> f
    )
  )
let run_c_parser =
  run_c_parser_clang;
  (*run_c_parser_memcad;*)
;;

let tests_ind =
  [ "bench/ind.lst" ;
    "bench/ind.tree" ;
    "bench/ind.length";
    "bench/ind.setin";
    "bench/ind.setex";
    "bench/ind.graph" ]
let test_ind_parser ( ) =
  meta_test_parser "inductive definitions"
    Ind_parser.main_indlist Ind_lexer.token
    (fun i ->
      if !test_verbose then List.iter (Ind_utils.pp_ind stdout) i)
    tests_ind

let tests_ilines =
  [ "bench/lsto.ind" ] @ tests_ind
let test_ilines_parser ( ) =
  meta_test_parser "inductive definitions + settings"
    Ind_parser.main_ilines Ind_lexer.token
    (fun i -> ( ))
    tests_ilines

let test_all_parsers ( ) =
  test_ind_parser ( );
  Printf.printf "\n"


(** Triggers  *)
(* Analysis *)
let do_analyze
    (indfile: string option) (* input file for inductive definitions *)
    (filename: string)       (* input C file *)
    (shapedom_struct: shape_dom) (* shape domain structure *)
    (mainfun: string)        (* main function *)
    : unit =
  begin
    match indfile with
    | None -> ( ) (* analysis with no inductive definition *)
    | Some ifile ->
        (* some inductive definitions to parse beforehand *)
        let l = ilines_parser ifile in
        Ind_utils.ind_init l;
        (* antoine: do the inductive defs pre-analysis *)
        if !reduction_mode_selector != Rm_disabled then
          begin
            T.app1 "ind_preds_init" Ind_analysis.ind_preds_init ();
            T.app1 "seg_preds_init" Ind_analysis.seg_preds_init ()
          end
  end;
  (* numerical domain construction *)
  let mod_apron = 
    match !nd_selector with
    | ND_box -> (module Nd_apron.N_box: Nd_sig.DOM_NUM_NB)
    | ND_oct -> (module Nd_apron.N_oct: Nd_sig.DOM_NUM_NB)
    | ND_pol -> (module Nd_apron.N_pol: Nd_sig.DOM_NUM_NB) in
  let module MApron  = (val mod_apron: Nd_sig.DOM_NUM_NB) in
  let module MAdiseq = Add_diseqs.Add_diseqs( MApron ) in
  (* AT: experimental: add_dyn_svenv.ml functor *)
  let module MAde    = Add_dyn_svenv.Add_dyn_svenv( MAdiseq ) in
  let module MNum    = Add_bottom.Add_bottom( MAde ) in
  let module MVal0   = Dom_val_num.Make_val_num( MNum ) in
  let mod_val1 =
    if !enable_submem then
      let module MSub = Dom_val_subm.Submem in
      (module Dom_val_subm.Make_Val_Subm( MVal0 )( MSub ): Dom_sig.DOM_VALUE)
    else
      (module MVal0: Dom_sig.DOM_VALUE) in
  let mod_val2 =
    if !timing_value then
      let module Mv = (val mod_val1: Dom_sig.DOM_VALUE) in
      (module Dom_val_num.Dom_val_timing( Mv ): Dom_sig.DOM_VALUE)
    else mod_val1 in
  let module MVal1 = (val mod_val2: Dom_sig.DOM_VALUE) in
  (* shape domain construction *)
  let add_timing m r os =
    let module D = (val m: Dom_sig.DOM_MEM_LOW) in
    begin
      match os with
      | None -> ( )
      | Some s -> D.init_inductives s
    end;
    let m =
      if !r then
        (module Dom_timing.Dom_mem_low_timing( D ): Dom_sig.DOM_MEM_LOW)
      else (module D: Dom_sig.DOM_MEM_LOW) in
    m in
  let build_shape_ind (s: StringSet.t) =
    Printf.printf "Initializing a summarizing shape domain <%d inductives>\n"
      (StringSet.cardinal s);
    let m = (module Dom_mem_low_shape.DBuild( MVal1 ): Dom_sig.DOM_MEM_LOW) in
    add_timing m timing_bshape (Some s) in
  let build_shape_flat () =
    Printf.printf "Creating flat shape domain (with no summarization)\n";
    let m = (module Dom_mem_low_flat.DBuild( MVal1 ): Dom_sig.DOM_MEM_LOW) in
    add_timing m timing_fshape None in
  let build_shape_list () =
    Printf.printf "Creating ad-hoc list domain\n";
    let m = (module Dom_mem_low_list.DBuild( MVal1 ): Dom_sig.DOM_MEM_LOW) in
    add_timing m timing_lshape None in
  let rec sd_2str = function
    | Shd_flat -> "[___]"
    | Shd_all -> "[@ll]"
    | Shd_list -> "[#list]"
    | Shd_inds l ->
        Printf.sprintf "[%s]" (gen_list_2str "" (fun x -> x) "," l)
    | Shd_sep (d0, d1) ->
        Printf.sprintf "(%s * %s)" (sd_2str d0) (sd_2str d1)
    | Shd_prod (d0, d1) ->
        Printf.sprintf "(%s X %s)" (sd_2str d0) (sd_2str d1) in
  let rec build_domain ind (sd: shape_dom) =
    Printf.printf "%sBuild_domain called %s\n" ind (sd_2str sd);
    match sd with
    | Shd_flat ->
        build_shape_flat ()
    | Shd_all ->
        let inds =
          StringMap.fold
            (fun s _ -> StringSet.add s) !Ind_utils.ind_defs StringSet.empty in
        build_shape_ind inds
    | Shd_list ->
        build_shape_list ()
    | Shd_inds lst ->
        let inds =
          List.fold_left
            (fun acc i ->
              Printf.printf
                "%sBuild domain adds inductive %s to domain\n" ind i;
              assert (StringMap.mem i !Ind_utils.ind_defs);
              StringSet.add i acc
            ) StringSet.empty lst in
        build_shape_ind inds
    | Shd_sep (d0, d1) ->
        let nind = "   " ^ ind in
        let m0 = build_domain nind d0 in
        let module D0 = (val m0: Dom_sig.DOM_MEM_LOW) in
        let m1 = build_domain nind d1 in
        let module D1 = (val m1: Dom_sig.DOM_MEM_LOW) in
        let module D = Dom_mem_low_sep.Dom_sep( D0 )( D1 ) in
        if !timing_sshape then
          (module Dom_timing.Dom_mem_low_timing( D ): Dom_sig.DOM_MEM_LOW)
        else
          (module D: Dom_sig.DOM_MEM_LOW)
    | Shd_prod (d0, d1) ->
        let nind = "   " ^ ind in
        let m0 = build_domain nind d0 in
        let module D0 = (val m0: Dom_sig.DOM_MEM_LOW) in
        let m1 = build_domain nind d1 in
        let module D1 = (val m1: Dom_sig.DOM_MEM_LOW) in
        let module D = Dom_mem_low_prod.Dom_prod( D0 )( D1 ) in
        if !timing_pshape then
          (module Dom_timing.Dom_mem_low_timing( D ): Dom_sig.DOM_MEM_LOW)
        else
          (module D: Dom_sig.DOM_MEM_LOW) in
  let mshape = build_domain "" shapedom_struct in
  let module MShape = (val mshape: Dom_sig.DOM_MEM_LOW) in
  (* Memory domain, expressions layer *)
  let module MMExprs = Dom_mem_exprs.DBuild( MShape ) in
  let m =
    if !timing_mem_exprs then
      (module 
          Dom_mem_exprs.Dom_mem_exprs_timing( MMExprs ): Dom_sig.DOM_MEM_EXPRS)
    else (module MMExprs: Dom_sig.DOM_MEM_EXPRS) in
  let module MMExprs = (val m: Dom_sig.DOM_MEM_EXPRS) in
  (* Environment domain *)
  let module MEnv    = Dom_env.Dom_env( MMExprs ) in
  let m =
    if !timing_env then
      (module Dom_env.Dom_env_timing( MEnv ): Dom_sig.DOM_ENV)
    else (module MEnv: Dom_sig.DOM_ENV) in
  let module MEnv = (val m: Dom_sig.DOM_ENV) in
  (* Disjunction domain *)
  let mod_disj =
    if !disj_selector then
      (module Dom_disj.Dom_disj( MEnv ): Dom_sig.DOM_DISJ)
    else
      (module Dom_no_disj.Dom_no_disj( MEnv ): Dom_sig.DOM_DISJ) in
  let module MDisj   = (val mod_disj: Dom_sig.DOM_DISJ) in
  (* C domain and analyzer *)
  let module MC      = Dom_c.Dom_C( MDisj ) in
  let module MAnalyzer = C_analyze.Make( MC ) in
  (* activation of the analysis *)
  let time_start = Unix.gettimeofday () in
  run_c_parser
    (fun cp ->
      let cp = C_process.process_c_prog cp in
      if !test_verbose then C_utils.ppi_c_prog "    " stdout cp ;
      if do_live_analysis then
        ignore (C_pre_analyze.live_prog mainfun cp);
      pp_config_report ();
      T.app2 "analyze" MAnalyzer.analyze_prog mainfun cp
    ) [ filename ];
  pipe_end_status_report ( );
  show_gc_statistics ( );
  Timer.print_timing_infos ();
  let time_finish = Unix.gettimeofday () in
  show_end_status_report (time_finish -. time_start)
(* Inference of inductive definitions *)
let do_infer_ind (filename: string): unit =
  run_c_parser
    (fun cp ->
      let cp = C_process.process_c_prog cp in
      if !test_verbose then C_utils.ppi_c_prog "    " stdout cp ;
      let l = Ind_infer.compute_inductives cp in
      Printf.printf "Computed inductive definitions:\n\n";
      List.iter (Ind_utils.pp_ind stdout) l) 
    [ filename ]


(** Pre-analysis configuration *)
let configure_pp (add_pps: string list) (rem_pps: string list): unit =
  let get_ref: string -> bool ref list = function
    (* global switches *)
    | "pp_is_le" ->
        [ flag_debug_is_le_gen; flag_debug_is_le_shape;
          flag_debug_is_le_num; flag_debug_is_le_strategy ]
    | "pp_join" ->
        [ flag_debug_join_gen; flag_debug_join_shape;
          flag_debug_join_num; flag_debug_join_strategy ]
    (* pretty-printing stuffs *)
    | "pp_nodeinfos"            -> [ flag_pp_nodeinfos ]
    | "debug_back_index"        -> [ flag_debug_back_index ]
    (* debugging *)
    | "pp_debug_materialize"    -> [ flag_debug_materialize ]
    | "pp_debug_unfold"         -> [ flag_debug_unfold ]
    | "pp_debug_bwd_unfold"     -> [ flag_debug_bwd_unfold ]
    | "pp_debug_trigger_unfold" -> [ flag_debug_trigger_unfold ]
    | "pp_debug_disj"           -> [ flag_debug_disj ]
    | "pp_debug_dommem_eval"    -> [ flag_debug_dommem_eval ]
    | "pp_debug_guard"          -> [ flag_debug_guard ]
    | "pp_debug_assign"         -> [ flag_debug_assign ]
    | "pp_debug_num_env"        -> [ flag_debug_num_env ]
    | "pp_debug_reduction"      -> [ flag_debug_reduction ]
    | "pp_display_num_env"      -> [ flag_display_num_env ]
    (* status at various statements *)
    | "pp_status_decl"          -> [ flag_status_decl ]
    | "pp_status_block"         -> [ flag_status_block ]
    (* statistics *)
    | "stats"                   -> [ enable_stats ]
    (* other cases are left as an error for now *)
    | s -> error (Printf.sprintf "configure_pp: unknown string %S" s) in
  List.iter (fun s -> List.iter (fun r -> r := true ) (get_ref s)) add_pps;
  List.iter (fun s -> List.iter (fun r -> r := false) (get_ref s)) rem_pps


(** Start *)
type mode_parsers =
  | Mpars_ind | Mpars_ilines
type mode =
  | M_basic
  | M_parse of mode_parsers
  | M_analyze
  | M_infer_ind
let main ( ): unit =
  (* References *)
  let mode: mode option ref = ref None in
  let indfile: string option ref = ref None in
  let filename: string ref = ref "" in
  let add_pps: string list ref = ref [] in
  let rem_pps: string list ref = ref [] in
  (* Setters functions *)
  let set_mode (m: mode) (): unit = mode := Some m in
  let set_numdom (nd: num_dom) (): unit = nd_selector := nd in
  let set_red_mode (rm: reduction_mode) (): unit =
    reduction_mode_selector:= rm in
  let set_stringopt (r: string option ref) (f: string): unit = r := Some f in
  let set_domstruct str = shapedom_struct := str in
  let add_submem_ind s = submem_inds := StringSet.add s !submem_inds in
  let add_pp s = add_pps := s :: !add_pps in
  let rem_pp s = rem_pps := s :: !rem_pps in
  let add_time s =
    let r =
      match s with
      | "apron"  -> timing_apron
      | "base"   -> timing_bshape
      | "flat"   -> timing_fshape
      | "prod"   -> timing_pshape
      | "sep"    -> timing_sshape
      | "mexprs" -> timing_mem_exprs
      | "env"    -> timing_env
      | "value"  -> timing_value
      | _ -> error (Printf.sprintf "unbound module to time: %s" s) in
    r := true in
  let add_widen_thr i =
    widen_thresholds := IntSet.add i !widen_thresholds in
  (* Parsing of arguments *)
  Arg.parse
    [ (* test mode *)
      "-basic-tests", Arg.Unit (set_mode M_basic), "series of basic tests";
      "-p-ind",       Arg.Unit (set_mode (M_parse Mpars_ind)), "inductives";
      "-a",           Arg.Unit (set_mode M_analyze), "analysis";
      "-stats",       Arg.Set enable_stats, "statistics";
      "-silent",      Arg.Unit debug_disable, "silent testing";
      "-very-silent", Arg.Unit very_silent, "very silent testing, for timing";
      "-v",           Arg.Set test_verbose, "verbose testing";
      "-main",        Arg.String (fun s -> mainfun := s), "main function";
      "-set-on",      Arg.String add_pp, "enable boolean settings";
      "-set-off",     Arg.String rem_pp, "disable boolean settings";
      (* timing *)
      "-timing",      Arg.String add_time, "perform timing of a module";
      (* inductive definitions settings *)
      "-use-ind",     Arg.String (set_stringopt indfile), "inductive file";
      "-ind-analysis",Arg.Set flag_indpars_analysis, "turns on ind analysis";
      "-infer-ind",   Arg.Unit (set_mode M_infer_ind), "guess inductives";
      (* structure of the shape domain *)
      "-shape-dom",   Arg.String set_domstruct, "shape domain structure";
      (* numeric domain selection *)
      "-nd-box",      Arg.Unit (set_numdom ND_box), "selects box domain";
      "-nd-oct",      Arg.Unit (set_numdom ND_oct), "selects oct domain";
      "-nd-pol",      Arg.Unit (set_numdom ND_pol), "selects pol domain";
      (* value domain configuration *)
      "-add-submem",  Arg.Set enable_submem, "enables sub-memory abstraction";
      "-submem-ind",  Arg.String add_submem_ind, "sub-memory inductive";
      (* disjunction domain activation *)
      "-disj-on",     Arg.Set disj_selector, "disjunctive domain on";
      "-disj-off",    Arg.Clear disj_selector, "disjunctive domain off";
      (* iteration strategy and widening parameters *)
      "-w-thr",       Arg.Set widen_do_thr, "activate threshold widening";
      "-w-no-thr",    Arg.Clear widen_do_thr, "deactivate threshold widening";
      "-w-add-thr",   Arg.Int add_widen_thr, "add widening threshold";
      "-j-iters",     Arg.Int (fun i -> join_iters:=i), "# of join iters";
      "-dw-iters",    Arg.Int (fun i -> dweak_iters := i), "# dir. weak iters";
      "-unrolls",     Arg.Int (fun i -> unrolls := i), "# of unroll iters";
      "-no-unroll-in",Arg.Clear unroll_inner, "deactivate inner loops unroll";
      "-part-lfps",   Arg.Set part_through_lfps, "partition through loops";
      "-no-part-lfps",Arg.Clear part_through_lfps, "no partition through loops";
      "-unary-abs",   Arg.Set do_unary_abstraction, "turns on unary abs";
      "-no-unary-abs",Arg.Clear do_unary_abstraction, "turns off unary abs";
      "-no-fast-iir", Arg.Clear do_quick_ind_ind_mt, "disable fast ind-ind emp";
      (* reduction *)
      "-red-disabled",Arg.Unit (set_red_mode Rm_disabled), "reduction disabled";
      "-red-manual",  Arg.Unit (set_red_mode Rm_manual), "man reduction";
      "-red-min",     Arg.Unit (set_red_mode Rm_minimal), "min reduction";
      "-red-on-read", Arg.Unit (set_red_mode Rm_on_read), "on read reduction";
      "-red-on-r-u",  Arg.Unit (set_red_mode Rm_on_read_and_unfold), 
      "on read and unfold reduction mode";
      "-red-max",     Arg.Unit (set_red_mode Rm_maximal), "max reduction";
      (* internal domain settings *)
      "-no-full-gc",  Arg.Clear flag_gc_full, "deactivate the full GC";
      (* latex output (turned off for regression tests) *)
      "-no-latex",    Arg.Clear flag_latex_output, "deactivate latex output";
      (* sending the results on a pipe *)
      "-pipe",        Arg.Set use_pipe, "communication of results on a pipe";
    ] (fun s -> filename := s) "";
  let do_all_tests () =
    test_all_parsers ( ) in
  configure_pp !add_pps !rem_pps;
  match !mode with
  | None -> do_all_tests ( )
  | Some m ->
      match m with
      | M_basic              -> do_all_tests ( )
      | M_parse Mpars_ind    -> test_ind_parser ( )
      | M_parse Mpars_ilines -> test_ilines_parser ( )
      | M_analyze            ->
          let sd =
            analyzed_file := !filename;
            let s = !shapedom_struct in
            let n = String.length s in
            assert (n > 2);
            let s = String.sub s 1 (n-2) in
            Printf.printf "Parsing string: %s\n" s;
            read_from_string "domain structure"
              Domsel_parser.edomshape Domsel_lexer.token s in
          do_analyze !indfile !filename sd !mainfun
      | M_infer_ind          ->
          do_infer_ind !filename
let _ = ignore (main ( ))
