(* Very beginnings of a basic C++ code generation module *)
(* Clients can construct a C++ AST and this module serializes*)
(* it to source code. *)

module Log = Logger.Make(struct let tag = "codegen" end)


(*****************************************************
 * C++ data types
 *****************************************************)


type enum_element = string * int option
  deriving (Show)


type enum_intf = {
  enum_name : string;
  enum_elements : enum_element list;
} deriving (Show)


type flag =
  | Explicit
  | Virtual
  | Static
  | Const
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
  decl_flags : flag list;
  decl_type : cpp_type;
  decl_name : string;
  decl_init : string;
} deriving (Show)

let empty_decl = {
  decl_flags = [];
  decl_type = TyVoid;
  decl_name = "";
  decl_init = "";
}


type class_member =
  | MemberField
    of (* field-decl *)declaration
  | MemberFunction
    of (* flags *)flag list
     * (* return-type *)cpp_type
     * (* name *)string
     * (* arguments *)declaration list
  | MemberConstructor
    of (* flags *)flag list
     * (* arguments *)declaration list
     * (* init-list *)(string * string) list
  deriving (Show)


type class_intf = {
  class_name : string;
  class_bases : string list;
  class_fields : class_member list;
  class_methods : class_member list;
} deriving (Show)


type intf =
  | Enum of enum_intf
  | Class of class_intf
  deriving (Show)



(*****************************************************
 * Stringification of some of the above.
 *****************************************************)


let string_of_flag = function
  | Explicit -> "explicit"
  | Virtual -> "virtual"
  | Static -> "static"
  | Const -> "const"

let rec string_of_cpp_type = function
  | TyInt -> "int"
  | TyName name -> name
  | TyPointer ty -> string_of_cpp_type ty ^ "*"
  | TyTemplate (template, ty) -> template ^ "<" ^ string_of_cpp_type ty ^ ">"
  | ty ->
      Log.unimp "type: %a"
        Show.format<cpp_type> ty


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


let flush cg : unit =
  Formatx.pp_print_flush cg.output ()


(*****************************************************
 * Interface
 *****************************************************)


let emit_enum_element fmt (name, value) =
  match value with
  | None -> Formatx.fprintf fmt "%s" name
  | Some i -> Formatx.fprintf fmt "%s = %d" name i


let emit_enum_intf fmt (i : enum_intf) =
  let pp_enum_list =
    Formatx.pp_list ~sep:Formatx.pp_comma_sep emit_enum_element
  in
  Formatx.fprintf fmt
    "@[<v>enum %s@,{@,@[<v2>  %a@]@,}@]"
    i.enum_name
    pp_enum_list i.enum_elements


let emit_flag fmt flag =
  Format.pp_print_string fmt (string_of_flag flag ^ " ")


let emit_type fmt ty =
  Format.pp_print_string fmt (string_of_cpp_type ty ^ " ")


let emit_declaration fmt decl =
  Formatx.fprintf fmt "%a%s"
    emit_type decl.decl_type
    decl.decl_name


let pp_argument_list =
  Formatx.pp_list ~sep:Formatx.pp_comma_sep emit_declaration

let pp_flag_list =
  Formatx.pp_list ~sep:(Formatx.pp_sep "") emit_flag

let pp_initialiser fmt = function
  | "" -> ()
  | init ->
      Format.pp_print_string fmt " = ";
      Format.pp_print_string fmt init


let emit_class_member_intf class_name fmt = function
  | MemberField { decl_flags; decl_type; decl_name; decl_init; } ->
      Formatx.fprintf fmt "%a%a%s%a;"
        pp_flag_list decl_flags
        emit_type decl_type
        decl_name
        pp_initialiser decl_init
  | MemberFunction (flags, retty, name, args) ->
      Formatx.fprintf fmt "%a%a%s (%a);"
        pp_flag_list flags
        emit_type retty
        name
        pp_argument_list args
  | MemberConstructor (flags, args, init) ->
      Formatx.fprintf fmt "%a%s (%a);"
        pp_flag_list flags
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
  


let emit_class_intf fmt (i : class_intf) : unit =
  (*Log.unimp "emit_class_intf: %s"*)
    (*(Show.show<class_intf> i)*)
  let pp_member_list =
    Formatx.pp_list ~sep:(Formatx.pp_sep "") (emit_class_member_intf i.class_name)
  in
  Formatx.fprintf fmt
    "@[<v>struct %s%a@,{@,@[<v2>  %a@]@,}@]"
    i.class_name
    emit_base_classes i.class_bases
    pp_member_list (i.class_fields @ i.class_methods)


let emit_intf fmt = function
  | Enum i -> emit_enum_intf fmt i
  | Class i -> emit_class_intf fmt i


let emit_intfs cg cpp_types =
  let pp_intf_list =
    Formatx.pp_list ~sep:(Formatx.pp_sep ";\n") emit_intf
  in
  Formatx.fprintf cg.output
    "@[<v>%a;@]@."
    pp_intf_list cpp_types


(*****************************************************
 * Implementation
 *****************************************************)


let emit_function_decl fmt (class_name, func_name, args) =
  Formatx.fprintf fmt "%s::%s (%a)@\n"
    class_name
    func_name
    pp_argument_list args


let emit_function_body fmt () =
  Formatx.fprintf fmt "@[<v2>{@,a@]@\n}"


let emit_class_member_impl class_name fmt = function
  | MemberFunction (flags, retty, name, args) ->
      Formatx.fprintf fmt "%a%a%a"
        emit_type retty
        emit_function_decl (class_name, name, args)
        emit_function_body ()
  | MemberConstructor (flags, args, init) ->
      Formatx.fprintf fmt "%a%a"
        emit_function_decl (class_name, class_name, args)
        emit_function_body ()
  | MemberField { decl_flags; decl_type; decl_name; decl_init; } ->
      Log.bug "fields have no implementation"


let emit_class_impl fmt (i : class_intf) : unit =
  Formatx.fprintf fmt "@[<v>@,%a@]"
    (Formatx.pp_list ~sep:(Formatx.pp_sep "\n")
      (emit_class_member_impl i.class_name)) i.class_methods


let emit_impl fmt = function
  | Enum i -> () (* enums have no implementation *)
  | Class i -> emit_class_impl fmt i


let emit_impls cg cpp_types =
  let pp_impl_list =
    Formatx.pp_list ~sep:(Formatx.pp_sep "\n") emit_impl
  in
  Formatx.fprintf cg.output
    "@[<v>%a@]@."
    pp_impl_list cpp_types
