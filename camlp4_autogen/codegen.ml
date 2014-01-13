(* Very beginnings of a basic C++ code generation module *)
(* Clients can construct a C++ AST and this module serializes*)
(* it to source code. *)

module Log = Logger.Make(struct let tag = "codegen" end)


(*****************************************************
 * C++ data types
 *****************************************************)


type enum_element = string * int option


type enum_interface = {
  enum_name : string;
  enum_elements :  enum_element list;
}


type cpp_type =
  | VoidType
  | EnumType of string
  | PointerType of string
  | ListType of string


type class_interface_member =
  | MemberField of (* field_type *)cpp_type * (* field_name *)string
  | MemberFunction of cpp_type * string * cpp_type list
  | MemberConstructor of string * cpp_type list


type class_interface = {
  class_name : string;
  class_interface_public_members : class_interface_member;
  class_interface_private_members : class_interface_member;
}


type interface =
  | Enum of enum_interface
  | Class of class_interface



(*****************************************************
 * Code generation
 *****************************************************)


type codegen_state = {
  codegen_output : Format.formatter;
}

type t = codegen_state


let make_codegen_with_channel (oc : out_channel) : t =
  let formatter = Formatx.formatter_of_out_channel oc in
  {codegen_output = formatter}


let make_codegen (path : string) : t =
  let oc = open_out path in
  make_codegen_with_channel oc


let emit_enum_element fmt (element: string * int option) =
  let (name, value) = element in
  match value with
  | None ->  Formatx.fprintf fmt "%s" name
  | Some i -> Formatx.fprintf fmt "%s = %d" name i


let emit_enum_interface (cg: t) (i : enum_interface) =
  let pp_enum_list = Formatx.pp_list ~sep:Formatx.pp_comma_sep emit_enum_element in
  Formatx.fprintf cg.codegen_output
    "enum %s {@\n  @[<v>%a@]@\n};@\n"
    i.enum_name pp_enum_list
    i.enum_elements


let emit_class_interface (cg: t) (i : class_interface) : unit =
  Log.unimp "emit_class_interface"


let emit_interface cg = function
  | Enum i -> emit_enum_interface cg i
  | Class i -> emit_class_interface cg i


let flush (cg : t) : unit =
  Formatx.pp_print_flush cg.codegen_output ()
