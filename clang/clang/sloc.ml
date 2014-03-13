type t = int
  deriving (Show)

type presumed_loc = {
  loc_filename : string;
  loc_line     : int;
  loc_column   : int;
} deriving (Show)


let invalid_presumed = {
  loc_filename = "";
  loc_line     = -1;
  loc_column   = -1;
}


let is_valid sloc =
  sloc <> 0

let is_valid_presumed ploc =
  ploc <> invalid_presumed
