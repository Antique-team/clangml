open Ast

let is_valid sloc =
  sloc.loc_s_line <> 0
