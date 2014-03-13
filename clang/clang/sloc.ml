let version = "$Id$"

type t = private int

module Show_t = Deriving_Show.Defaults(struct

    type a = t

    let format fmt (a : a) =
      Deriving_Show.Show_int.format fmt (a :> int)

  end)

type characteristic_kind =
  | C_User
  | C_System
  | C_ExternCSystem

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


let is_valid (sloc : t) =
  (sloc :> int) <> 0

let is_valid_presumed ploc =
  ploc <> invalid_presumed
