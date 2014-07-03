(* Very beginnings of a basic C++ code generation module *)
(* Clients can construct a C++ AST and this module serializes*)
(* it to source code. *)

open Util

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
  | Inline
  | Const
  deriving (Show)


type cpp_type =
  (* Built-in basic types (or std:: types) *)
  | TyVoid
  | TyBool
  | TyChar
  | TyUChar
  | TyInt
  | TyFloat
  | TyString
  (* Typedef-name *)
  | TyName of string
  (* Parameterised types *)
  | TyConst of cpp_type
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


type expression =
  | IdExpr of string
  | IntLit of int
  | StrLit of string
  | FCall of (* name *)string * (* arguments *)expression list
  | New of cpp_type * expression list
  deriving (Show)


type statement =
  | CompoundStmt of statement list
  | Return of expression
  | Expression of expression
  deriving (Show)


let empty_body = CompoundStmt []

type member_function = {
  flags : flag list;
  retty : cpp_type;
  name : string;
  params : declaration list;
  this_flags : flag list;
  body : statement;
} deriving (Show)


type class_member =
  | MemberField
    of (* field-decl *)declaration
  | MemberFunction of member_function
  | MemberConstructor
    of (* flags *)flag list
     * (* arguments *)declaration list
     * (* init-list *)(string * string) list
     * (* body *)statement
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
  | Forward of string
  | Function of member_function
  | Variable of cpp_type * string * expression
  deriving (Show)



(*****************************************************
 * Stringification of some of the above.
 *****************************************************)


let string_of_flag = function
  | Explicit -> "explicit"
  | Virtual -> "virtual"
  | Static -> "static"
  | Inline -> "inline"
  | Const -> "const"

let rec string_of_cpp_type = function
  | TyBool -> "bool"
  | TyChar -> "char"
  | TyUChar -> "unsigned char"
  | TyInt -> "int"
  | TyFloat -> "float"
  | TyString -> "llvm::StringRef"
  | TyName name -> name
  | TyConst ty -> string_of_cpp_type ty ^ " const"
  | TyPointer ty -> string_of_cpp_type ty ^ "*"
  | TyReference ty -> string_of_cpp_type ty ^ "&"
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

let emit_type fmt ty =
  Format.pp_print_string fmt (string_of_cpp_type ty ^ " ")


let emit_declaration fmt decl =
  Formatx.fprintf fmt "%a%s"
    emit_type decl.decl_type
    decl.decl_name


let pp_param_list =
  Formatx.pp_list ~sep:Formatx.pp_comma_sep emit_declaration


let pp_this_flag_list =
  let emit_flag fmt flag =
    Format.pp_print_string fmt (" " ^ string_of_flag flag)
  in
  Formatx.pp_list ~sep:(Formatx.pp_sep "") emit_flag


let emit_function_decl fmt (class_name, func_name, params, this_flags) =
  let class_name =
    if class_name = "" then
      class_name
    else
      class_name ^ "::"
  in
  Formatx.fprintf fmt "%s%s (@[<v>%a@])%a@\n"
    class_name
    func_name
    pp_param_list params
    pp_this_flag_list this_flags


let emit_enum_element fmt (name, value) =
  match value with
  | None -> Formatx.fprintf fmt "%s" name
  | Some i -> Formatx.fprintf fmt "%s = %d" name i


let emit_enum_intf fmt (i : enum_intf) =
  let pp_enum_list =
    Formatx.pp_list ~sep:Formatx.pp_comma_sep emit_enum_element
  in
  Formatx.fprintf fmt
    "@[<v>enum %s@,\
     {@,\
     @[<v2>  %a@]@,\
     };@]"
    i.enum_name
    pp_enum_list i.enum_elements


let pp_flag_list =
  let emit_flag fmt flag =
    Format.pp_print_string fmt (string_of_flag flag ^ " ")
  in
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
  | MemberFunction { flags; retty; name; params; this_flags; body } ->
      Formatx.fprintf fmt "%a%a%s (%a)%a;"
        pp_flag_list flags
        emit_type retty
        name
        pp_param_list params
        pp_this_flag_list this_flags
  | MemberConstructor (flags, params, init, body) ->
      Formatx.fprintf fmt "%a%s (%a);"
        pp_flag_list flags
        class_name
        pp_param_list params


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
    "@[<v>struct %s%a@,\
     {@,\
     @[<v2>  %a@]@,\
     };@]"
    i.class_name
    emit_base_classes i.class_bases
    pp_member_list (i.class_fields @ i.class_methods)


let rec emit_expression fmt = function
  | IdExpr id ->
      Formatx.pp_print_string fmt id
  | IntLit value ->
      Formatx.pp_print_int fmt value
  | StrLit str ->
      Formatx.fprintf fmt "\"%s\"" (String.escaped str)
  | FCall (name, params) ->
      Formatx.fprintf fmt "%s (%a)"
        name
        pp_arguments params
  | New (ty, args) ->
      Formatx.fprintf fmt "new %a(%a)"
        emit_type ty
        pp_arguments args

and pp_arguments fmt =
  Formatx.pp_list ~sep:Formatx.pp_comma_sep
    emit_expression fmt


let rec emit_statement fmt = function
  | CompoundStmt stmts ->
      Formatx.fprintf fmt "@[<v2>{@,%a@]@\n}"
        (Formatx.pp_list ~sep:(Formatx.pp_sep "\n")
          emit_statement) stmts
  | Return expr ->
      Formatx.fprintf fmt "return %a;"
        emit_expression expr
  | Expression expr ->
      Formatx.fprintf fmt "%a;"
        emit_expression expr


let emit_intf fmt = function
  | Enum i -> emit_enum_intf fmt i
  | Class i -> emit_class_intf fmt i
  | Forward name ->
      Formatx.fprintf fmt "struct %s;" name
  | Variable (ty, name, init) ->
      Formatx.fprintf fmt "extern %a %s;"
        emit_type ty
        name
  | Function { flags; retty; name; params; this_flags; body } ->
      Formatx.fprintf fmt "%a%a%a%a"
        pp_flag_list flags
        emit_type retty
        emit_function_decl ("", name, params, [])
        emit_statement body


let emit_intfs basename cg cpp_types =
  let pp_intf_list =
    Formatx.pp_list ~sep:(Formatx.pp_sep "\n") emit_intf
  in
  let ucasename = String.uppercase basename in
  Formatx.fprintf cg.output
    "@[<v0>#ifndef %s_H@,\
     #define %s_H@,\
     #include \"clang_ref.h\"@,\
     //#include \"ocaml++.h\"@,\
     @,\
     namespace %s {@,\
     %a@,\
     }@,\
     @,\
     #endif /* %s_H */@]@."
    (* #ifndef *) ucasename
    (* #define *) ucasename
    (* namespace *) basename
    pp_intf_list cpp_types
    (* #endif *) ucasename


(*****************************************************
 * Implementation
 *****************************************************)


let emit_ctor_init fmt (member, initialiser) =
  Formatx.fprintf fmt "%s (%s)"
    member initialiser


let emit_ctor_init_list fmt = function
  | [] -> ()
  | init ->
      let pp_init_list =
        Formatx.pp_list ~sep:Formatx.pp_comma_sep emit_ctor_init
      in
      Formatx.fprintf fmt "  : %a@\n"
        pp_init_list init


let emit_class_member_impl class_name fmt = function
  | MemberFunction { flags; retty; name; params; this_flags; body } ->
      Formatx.fprintf fmt "%a%a%a"
        emit_type retty
        emit_function_decl (class_name, name, params, this_flags)
        emit_statement body
  | MemberConstructor (flags, params, init, body) ->
      Formatx.fprintf fmt "%a%a%a"
        emit_function_decl (class_name, class_name, params, [])
        emit_ctor_init_list init
        emit_statement body
  | MemberField { decl_flags; decl_type; decl_name; decl_init; } ->
      Log.bug "fields have no implementation"


let emit_class_impl fmt (i : class_intf) : unit =
  Formatx.fprintf fmt "@[<v>@,%a@]"
    (Formatx.pp_list ~sep:(Formatx.pp_sep "\n")
      (emit_class_member_impl i.class_name)) i.class_methods


let emit_impl fmt = function
  | Class i -> emit_class_impl fmt i
  | Variable (ty, name, init) ->
      Formatx.fprintf fmt "%a %s = %a;"
        emit_type ty
        name
        emit_expression init
  (* enums and forward declarations have no implementation *)
  | Function _ | Forward _ | Enum _ -> ()


let emit_impls basename cg cpp_types =
  let pp_impl_list =
    Formatx.pp_list ~sep:(Formatx.pp_sep "") emit_impl
  in
  Formatx.fprintf cg.output
    "#include \"%s.h\"@\n\
     #include \"ocaml++/bridge.h\"@,\
     namespace %s {@\n\
     @[<v>%a@]@,\
     }@."
    basename
    basename
    pp_impl_list cpp_types
