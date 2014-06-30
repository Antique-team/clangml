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
  (* 'a Id (index in TypedArrays) *)
  | RefType of loc * basic_type
  (* List of basic_type *)
  | ListOfType of loc * basic_type
  (* Optional basic_type *)
  | OptionType of loc * basic_type
  (* Will want others, eventually *)
  deriving (Show)

type sum_type_branch = loc * (* branch_name *)string * (* types *)basic_type list
  deriving (Show)

type sum_type = loc * (* sum_type_name *)string * (* branches *)sum_type_branch list
  deriving (Show)

type record_member = loc * (* name *)string * (* type *)basic_type
  deriving (Show)

type record_type = loc * (* name *)string * (* members *)record_member list
  deriving (Show)

type ocaml_type =
  | AliasType of loc * string * basic_type
  | SumType of sum_type
  | RecordType of record_type
  | RecursiveType of loc * ocaml_type list
  | Version of loc * string
  deriving (Show)


type type_map = (* name *)string * record_type * sum_type
  deriving (Show)
