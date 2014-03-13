open Clang


type standard =
  | Std_C of string
  | Std_CXX of string

let string_of_standard = function
  | Std_C   paragraph -> "ISO C99 §" ^ paragraph
  | Std_CXX paragraph -> "ISO C++ §" ^ paragraph


type flag =
  | Wreserved_name

let string_of_flag = function
  | Wreserved_name -> "-Wreserved-name"


type level =
  | Warning
  | Error

let string_of_level = function
  | Warning -> "warning"
  | Error   -> "error"


type diagnostic = {
  diag_loc : Sloc.t;
  diag_msg : string;
  diag_std : standard;
  diag_flg : flag;
  diag_lvl : level;
}


let show clang diag =
  let ploc = Api.(request clang @@ PresumedLoc diag.diag_loc) in

  let msg = ANSITerminal.([
    [Bold; white], Printf.sprintf "%s:%d:%d: "
      ploc.Sloc.loc_filename
      ploc.Sloc.loc_line
      ploc.Sloc.loc_column;
    [Bold; magenta], Printf.sprintf "%s: "
      (string_of_level diag.diag_lvl);
    [Bold; white], Printf.sprintf "%s [%s, %s]\n"
      diag.diag_msg
      (string_of_flag diag.diag_flg)
      (string_of_standard diag.diag_std);
  ]) in

  List.iter (fun (style, msg) ->
    ANSITerminal.prerr_string style msg
  ) msg
