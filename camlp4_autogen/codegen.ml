(* Very beginnings of a basic C++ code generation module *)
(* Clients can construct a C++ AST and this module serializes*)
(* it to source code. *)

module Log = Logger.Make(struct let tag = "codegen" end)


(*****************************************************
 * C++ data types
 *****************************************************)


type enum_element = string * int option
  deriving (Show)


type enum_interface = {
  enum_name : string;
  enum_elements : enum_element list;
} deriving (Show)


type flag =
  | Virtual
  | Static
  deriving (Show)


type cpp_type =
  (* Built-in basic types (or std:: types) *)
  | TyVoid
  | TyInt
  | TyString
  (* Typedef-name *)
  | TyName of string
  (* Parameterised types *)
  | TyPointer of cpp_type
  | TyReference of cpp_type
  | TyTemplate of string * cpp_type
  deriving (Show)


type declaration = {
  decl_type : cpp_type;
  decl_name : string;
} deriving (Show)


type class_member =
  | MemberField
    of (* field-decl *)declaration
  | MemberFunction
    of (* flags *)flag list
     * (* return-type *)cpp_type
     * (* name *)string
     * (* arguments *)declaration list
  | MemberConstructor
    of (* arguments *)declaration list
  deriving (Show)


type class_interface = {
  class_name : string;
  class_bases : string list;
  class_members : class_member list;
} deriving (Show)


type interface =
  | Enum of enum_interface
  | Class of class_interface
  deriving (Show)



(*****************************************************
 * Code generation
 *****************************************************)


type codegen_state = {
  output : Format.formatter;
}

type t = codegen_state


let make_codegen_with_channel (oc : out_channel) : t =
  let formatter = Formatx.formatter_of_out_channel oc in
  { output = formatter }


let make_codegen (path : string) : t =
  let oc = open_out path in
  make_codegen_with_channel oc


let emit_enum_element fmt (name, value) =
  match value with
  | None -> Formatx.fprintf fmt "%s" name
  | Some i -> Formatx.fprintf fmt "%s = %d" name i


let emit_enum_interface fmt (i : enum_interface) =
  let pp_enum_list =
    Formatx.pp_list ~sep:Formatx.pp_comma_sep emit_enum_element
  in
  Formatx.fprintf fmt
    "@[<v>enum %s@,{@,@[<v2>  %a@]@,}@]"
    i.enum_name
    pp_enum_list i.enum_elements


let string_of_flag = function
  | Virtual -> "virtual"
  | Static -> "static"


let emit_flag fmt flag =
  Format.pp_print_string fmt (string_of_flag flag ^ " ")


let rec string_of_cpp_type = function
  | TyInt -> "int"
  | TyName name -> name
  | TyPointer ty -> string_of_cpp_type ty ^ "*"
  | TyTemplate (template, ty) -> template ^ "<" ^ string_of_cpp_type ty ^ ">"
  | ty ->
      Log.unimp "type: %a"
        Show.format<cpp_type> ty


let emit_type fmt ty =
  Format.pp_print_string fmt (string_of_cpp_type ty ^ " ")


let emit_declaration fmt decl =
  Formatx.fprintf fmt "%a%s"
    emit_type decl.decl_type
    decl.decl_name


let pp_argument_list =
  Formatx.pp_list ~sep:Formatx.pp_comma_sep emit_declaration


let emit_class_member class_name fmt = function
  | MemberField { decl_type; decl_name } ->
      Formatx.fprintf fmt "%a%s;"
        emit_type decl_type
        decl_name
  | MemberFunction (flags, retty, name, args) ->
      let pp_enum_list =
        Formatx.pp_list ~sep:(Formatx.pp_sep "") emit_flag
      in
      Formatx.fprintf fmt "%a%a%s (%a);"
        pp_enum_list flags
        emit_type retty
        name
        pp_argument_list args
  | MemberConstructor (args) ->
      Formatx.fprintf fmt "%s (%a);"
        class_name
        pp_argument_list args


let emit_base_classes fmt bases =
  let pp_base_list =
    Formatx.pp_list ~sep:Formatx.pp_comma_sep Format.pp_print_string
  in
  match bases with
  | [] -> ()
  | xs ->
      Formatx.fprintf fmt
        " : %a"
        pp_base_list xs
  


let emit_class_interface fmt (i : class_interface) : unit =
  (*Log.unimp "emit_class_interface: %s"*)
    (*(Show.show<class_interface> i)*)
  let pp_member_list =
    Formatx.pp_list ~sep:(Formatx.pp_sep "") (emit_class_member i.class_name)
  in
  Formatx.fprintf fmt
    "@[<v>struct %s%a@,{@,@[<v2>  %a@]@,}@]"
    i.class_name
    emit_base_classes i.class_bases
    pp_member_list i.class_members


let emit_interface fmt = function
  | Enum i -> emit_enum_interface fmt i
  | Class i -> emit_class_interface fmt i


let emit_interfaces cg cpp_types =
  let pp_interface_list =
    Formatx.pp_list ~sep:(Formatx.pp_sep ";\n") emit_interface
  in
  Formatx.fprintf cg.output
    "@[<v>%a;@]@."
    pp_interface_list cpp_types


let flush cg : unit =
  Formatx.pp_print_flush cg.output ()
