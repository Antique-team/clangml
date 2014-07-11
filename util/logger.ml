(** Logging.

    This module is built on the [Format] module. Each logging function
    is a [Format.printf]-like function.
    
    Logging modules are created by instantiating {!Make}.  For example,
    we can create a module [L] and use it to log a debugging message
    as follows:
    {[
    module L = Logger.Make(struct let tag = "main" end)
    let f () = L.dbg "%d" 42 ]}
    
    For each logging function, top-level text is automatically line wrapped.
    A newline and flush is also automatic.
*)

(** {2 Tags and levels} *)

(** Tags to associate with a module, a library, etc. *)
type tag = string

(** Logging levels. *) 
type loglevel = 
  | Dbg   (** Debugging messages. *)
  | Msg   (** User messages. *)
  | Warn  (** Warnings. *)
  | Err   (** Error messages. *)
  | Unimp (** Unimplemented feature. *)
  | Bug   (** Bug in the implementation. *)

(** Level numbering. *)
let loglevel_number: loglevel -> int = (function Dbg -> 0 | Msg -> 1 | Warn -> 2 | Err -> 3 | Unimp -> 4 | Bug -> 5)


(** {2 Global parameters} *)

(** Flag for showing tags. (default: true) *)
let show_tag: bool ref = ref false

(** Verbose flag.  If false, only print messages and above. (default: true) *)
let verbose: bool ref = ref true

(** A parameter to decide if we log the message.
    (default: based on {!verbose}) *)
let show_msg: (tag -> loglevel -> bool) ref = ref begin fun _ level ->
    if !verbose then true
    else (loglevel_number level) >= (loglevel_number Msg)
  end

(** A parameter for printing location information. (default: no location printing) *)
let pp_currloc_option: ((Format.formatter -> unit -> unit) option) ref = ref None

(** Width of the tag column in text mode. (default: 10) *)
let tag_width: int ref = ref 10

(** Message formatter. (default: stdout) *)
let message_formatter: Format.formatter ref = ref Format.std_formatter

(** Set or get error formatter. (default: stderr) *)
let (set_error_formatter: out_channel -> unit),
    (get_error_formatter: unit -> Format.formatter),
    (flush_errmsg_buffer: unit -> string) =
  (* gross imperative stuff *)
  let errmsg_buffer = Buffer.create 64 in
  let make_error_formatter (oc: out_channel) : Format.formatter =
    let output string pos len =
      (* buffer *)
      Buffer.add_substring errmsg_buffer string pos len;
      (* channel *)
      output oc string pos len;
      ()
    in
    let flush () = flush oc in
    Format.make_formatter output flush 
  in
  let error_formatter = ref (make_error_formatter stderr) in
  let set oc = error_formatter := make_error_formatter oc in
  let get () = !error_formatter in
  let flush_buffer () = let str = Buffer.contents errmsg_buffer in Buffer.clear errmsg_buffer; str in
  (set, get, flush_buffer)
  
  
(** {2 Errors} *)  

(** Error exception.  This exception is thrown when a fatal error has been logged. *)
exception Error of loglevel * string

(** Whether an error has occurred (i.e., {!Error} has been raised)? *)
let had_errors : bool ref = ref false


(** {2 Logging} *)


(** A logger parameter. *)
module type PARAM = sig
  (** A tag for the module, library, etc. *)
  val tag: string
end

(** The signature for a logger. *)
module type S = sig
  (** Print debugging messages. *)
  val dbg: ('a,Format.formatter,unit) format -> 'a
  
  (** Print user messages. *)
  val msg: ('a,Format.formatter,unit) format -> 'a

  (** Print warning messages. *)
  val warn: ('a,Format.formatter,unit) format -> 'a

  (** Print error messages. *)
  val err: ('a,Format.formatter,unit,'b) format4 -> 'a

  (** Print unimplemented messages. *)
  val unimp: ('a,Format.formatter,unit,'b) format4 -> 'a

  (** Print implementation bug (i.e., internal error) messages. *)
  val bug: ('a,Format.formatter,unit,'b) format4 -> 'a
end

(** Create a logger. *)
module Make (P: PARAM) : S = struct
  
  (** Tag restricted by {!tag_width}.  Prints only if {!show_tag} true. *)
  let pp_tag fmt (tag: tag) =
    let tag_text, tag_padding =
      let len = String.length P.tag in
      let width = !tag_width - 2 in
      if len < width then (P.tag, String.make (width - len) ' ')
      else (String.sub P.tag 0 width, "")
    in
    if !show_tag then Format.fprintf fmt "[%s]%s " tag_text tag_padding
  
  let pp_loglevel fmt = function
    | Dbg -> ()
    | Msg -> ()
    | Warn -> Format.fprintf fmt "[1;33mWarning:[0m@ "
    | Err -> Format.fprintf fmt "[1;31mError:[0m@ "
    | Unimp -> Format.fprintf fmt "[1;31mUnimplemented:[0m@ "
    | Bug -> Format.fprintf fmt "[1;31mBug:[0m@ "
  
  external format_of_string : string -> ('a, 'b, 'c, 'd) format4 = "%identity"
  
  (** Replace spaces by breaks in a format string. *)
  let break_spaces (fmt: ('a, 'b, 'c, 'd) format4) : ('a, 'b, 'c, 'd) format4 =
    (* gross imperative stuff *)
    let fmtstr = string_of_format fmt in
    let module BrkSp =
    struct
      type state = { n_open_box: int; at_last: bool }
      let init_state = { n_open_box = 0; at_last = false }
      let curr_state = ref init_state
      let reset_state () = curr_state := init_state
      let is_break_space_f (s: state) (c: char) : bool * state =
        if c = ' ' && s.n_open_box = 0 then (true, s)
        else
          let s =
            if c = '@' then { s with at_last = true }
            else if c = '[' && s.at_last then { n_open_box = s.n_open_box + 1; at_last = false }
            else if c = ']' && s.at_last then { n_open_box = s.n_open_box - 1; at_last = false }
            else { s with at_last = false }
          in
          (false, s)
      let is_break_space (c: char) : bool =
        let b, s = is_break_space_f (!curr_state) c in
        curr_state := s;
        b
    end
    in
    let nsp, n =
      let nsp_ref, n_ref = ref 0, ref 0 in
      let _ =
        String.iter (fun c -> incr n_ref; if BrkSp.is_break_space c then incr nsp_ref) fmtstr;
        BrkSp.reset_state ()
      in
      !nsp_ref, !n_ref
    in
    let fmtstr' = String.create (n + nsp) in
    let _ =
      let i = ref 0 in
      String.iter
        (fun c ->
              if BrkSp.is_break_space c then (fmtstr'.[!i] <- '@'; incr i; fmtstr'.[!i] <- ' ')
              else fmtstr'.[!i] <- c;
              incr i)
        fmtstr;
      BrkSp.reset_state ()
    in
    format_of_string fmtstr'
  
  (** Helper function for logging messages. *)
  let log k (level: loglevel) fmt fmtstring =
    let pp_currloc fmt () =
      match !pp_currloc_option with
        | None -> ()
        | Some pp -> Format.fprintf fmt "%a:@ " pp ()
    in
    (* Make all spaces in user-provide format string breaking. *)
    let fmtstring = break_spaces fmtstring in
    (* Prefix format string *)
    let fmtstring =
      "%a@[" ^^ (* tag *)
      "%a" ^^ (* location *)
      "%a" ^^ (* log level *)
      fmtstring ^^ "@]"
    in
    Format.kfprintf k fmt fmtstring
      pp_tag P.tag
      pp_currloc ()
      pp_loglevel level
      
  let null_buffer = Buffer.create 128
  let null_formatter = Format.formatter_of_buffer null_buffer
      
  (** Helper function for logging messages. *)
  let maybe_log (level: loglevel) fmt fmtstring =
    if !show_msg P.tag level then begin
      let continuation fmt = Format.pp_print_newline fmt () in
      log continuation level fmt fmtstring
    end
    else
      Format.kfprintf (fun _ -> Buffer.reset null_buffer) null_formatter fmtstring
  
  (** Helper function for logging and raising {!Error}. *)
  let fail (level: loglevel) fmtstring =
    had_errors := true;
    let fmt = get_error_formatter () in
    let continuation fmt =
      let _ = Format.pp_print_flush fmt () in
      let msg = flush_errmsg_buffer () in
      let _ = Format.pp_print_newline fmt (); ignore (flush_errmsg_buffer ()) in
      raise (Error (level, msg))
    in
    log continuation level fmt fmtstring
  
  (** {3 Public interface} *)
  
  let dbg fmtstring = maybe_log Dbg !message_formatter fmtstring
  let msg fmtstring = maybe_log Msg !message_formatter fmtstring
  let warn fmtstring = maybe_log Warn !message_formatter fmtstring
  let err fmtstring = fail Err fmtstring
  let unimp fmtstring = fail Unimp fmtstring
  let bug fmtstring = fail Bug fmtstring
end
