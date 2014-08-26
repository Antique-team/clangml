open Camlp4.PreCast


type loc = Ast.loc

module Show_loc = Deriving_Show.Defaults(struct
    type a = loc

    let format fmt loc =
      Format.fprintf fmt "<loc>"

  end)


type basic_type =
  (* Simple type *)
  | NamedType of loc * string
  (* Clang source location *)
  | SourceLocation of loc
  (* Clang pointer *)
  | ClangType of loc * string
  (* 'a DenseIntMap.key *)
  | RefType of loc * basic_type
  (* List of basic_type *)
  | ListOfType of loc * basic_type
  (* Optional basic_type *)
  | OptionType of loc * basic_type
  (* Tuple of basic_type *)
  | TupleType of loc * basic_type list
  deriving (Show)

type sum_type_branch = {
  stb_loc : loc;
  stb_name : string;
  stb_types : basic_type list;
} deriving (Show)

type sum_type = {
  st_loc : loc;
  st_name : string;
  st_branches : sum_type_branch list;
} deriving (Show)

type record_type_member = {
  rtm_loc : loc;
  rtm_name : string;
  rtm_type : basic_type;
} deriving (Show)

type record_type = {
  rt_loc : loc;
  rt_name : string;
  rt_members : record_type_member list;
} deriving (Show)

type ocaml_type =
  | AliasType of loc * string * basic_type
  | SumType of sum_type
  | RecordType of record_type
  | RecursiveType of loc * ocaml_type list
  | Version of loc * string
  deriving (Show)


type type_map = (* name *)string * record_type * sum_type
  deriving (Show)


type ocaml_types = (string * ocaml_type) list
  deriving (Show)
