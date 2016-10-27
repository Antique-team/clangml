open Ocamlbuild_plugin


module Vars = struct
  let clang_version = "3.5"
  let ocaml_version = Sys.ocaml_version
  let ocaml_ver = Filename.chop_extension ocaml_version
  let ocaml_dist = "ocaml-" ^ ocaml_version
  let ocaml_tar = ocaml_dist ^ ".tar.gz"
end

let string_of_list to_string sep l =
  "[" ^ String.concat sep (List.map to_string l) ^ "]"

let concat_strings (l: string list): string =
  string_of_list (fun x -> x) "; " l

type shell_command = string

(* read first line output by given command *)
let read_stdout (cmd: shell_command): string =
  let cmd_out = Unix.open_process_in cmd in
  let res =
    try String.trim (input_line cmd_out)
    with End_of_file -> ""
  in
  let () = close_in cmd_out in
  res

(* return full path of command, if found *)
let command_exists (cmd: string): string option =
  if Unix.system ("which " ^ cmd ^ " 2>&1 > /dev/null") = Unix.WEXITED 0 then
    Some (read_stdout ("which " ^ cmd))
  else
    None

type os_type = Linux | OSX

let get_os_type (): os_type =
  match read_stdout "uname" with
  | "Linux" -> Linux
  | "Darwin" -> OSX
  | _ -> failwith "get_os_type: OS is neither Linux nor OSX"

exception Command_found of string
exception No_command_found of string

let first_command_found (cmds: string list): string =
  try
    List.iter (fun cmd ->
      match command_exists cmd with
      | None -> ()
      | Some c -> raise (Command_found c)
    ) cmds;
    raise (No_command_found (concat_strings cmds))
  with Command_found c -> c

let cpp_compiler =
  first_command_found ["clang++-" ^ Vars.clang_version; (* linux *)
                       "/usr/local/bin/clang++-3.5"] (* osx *)

let llvm_config =
  first_command_found ["llvm-config-" ^ Vars.clang_version; (* linux *)
                       "/usr/local/bin/llvm-config-3.5"] (* osx *)

(* (\* enforce llvm-config and clang++ versions match *\) *)
(* let () = *)
(*   let llvm_config_version = Printf.sprintf "%s --version" llvm_config in *)
(*   if read_stdout llvm_config_version <> Vars.clang_long_version then *)
(*     failwith (Printf.sprintf "%s and %s versions differ" cpp_compiler llvm_config) *)

type _ prompt_question =
  | PQ_YN : [`PQ_YN] prompt_question

type _ prompt_answer =
  | PA_Y : [`PQ_YN] prompt_answer
  | PA_N : [`PQ_YN] prompt_answer

let prompt_string = function
  | PQ_YN -> "[Y/n]"

let prompt_answer : type a. a prompt_question -> string -> a prompt_answer =
  fun kind answer ->
    match kind with
    | PQ_YN ->
        (match String.lowercase_ascii answer with
         | "y" | "yes" | "" -> PA_Y
         | _ -> PA_N
        )

let prompt kind =
  Printf.kfprintf (fun out ->
    Printf.fprintf out " %s "
      (prompt_string kind);
    flush stdout;
    Scanf.fscanf stdin "%s" (prompt_answer kind)
  ) stdout


let pread cmd =
  let proc = Unix.open_process_in cmd in
  String.trim @@ input_line proc


let find_ocamlpicdir_no_opam () =
  let src =
    "http://caml.inria.fr/pub/distrib/ocaml-"
    ^ Vars.ocaml_ver
    ^ "/ocaml-" ^ Vars.ocaml_version
    ^ ".tar.gz"
  in

  (* download and extract *)
  if not (Sys.file_exists Vars.ocaml_dist &&
          Sys.is_directory Vars.ocaml_dist) then (
    if not (Sys.file_exists Vars.ocaml_tar) then
      if Sys.command @@ "wget " ^ src <> 0 then
        failwith "could not download ocaml source tarball";
    if Sys.command @@ "tar zxf " ^ Vars.ocaml_tar <> 0 then
      failwith "unable to extract ocaml source tarball";
  );

  Unix.chdir Vars.ocaml_dist;

  (* runtime *)
  if not (Sys.file_exists "asmrun/libasmrun_pic.a") then (
    if Sys.command "make world" <> 0 then
      failwith "unable to build ocaml runtime";
    if Sys.command "make world.opt" <> 0 then
      failwith "unable to build ocaml runtime";
  );

  (* install *)
  if not (Sys.file_exists "_install/lib/ocaml/libasmrun_pic.a") then (
    if Sys.command "make install" <> 0 then
      failwith "unable to install ocaml runtime";
  );

  Sys.getenv "PWD" ^ "/" ^ Vars.ocaml_dist ^ "/_install"

let find_pic_dir (): string =
  read_stdout "ocamlc -v | grep 'Standard library' | cut -d' ' -f4"

let find_ocamlpicdir () : string =
  try
    (* See if opam exists. *)
    let opam_dir = read_stdout "opam config var root" in
    if Sys.file_exists opam_dir then (
      let pic_dir = find_pic_dir () in
      let libasmrun_a = pic_dir ^ "/libasmrun_pic.a" in
      (* See if there is the library we need. *)
      if Sys.file_exists libasmrun_a then
        pic_dir
      (* No opam version of 4.01.0+PIC. Ask the user whether he wants
         to use opam to install one. *)
      else if prompt PQ_YN
          "Clang/ML needs a special PIC version of the OCaml runtime; we can \
           try to install it using OPAM.\nWould you like to attempt this?"
              = PA_Y then (
        (* Yes, try to install. *)
        let preferred = read_stdout "opam switch show" in
        if Sys.command @@ "opam switch " ^ Vars.ocaml_version ^ "+PIC" <> 0 then
          failwith "opam failed to switch to PIC compiler";
        if  Sys.command @@ "opam switch " ^ preferred <> 0 then
          failwith "opam failed to switch back to preferred compiler";
        (* Now our file should exist. *)
        if Sys.file_exists libasmrun_a then
          pic_dir
        else
          failwith "despite installing ocaml with PIC, the required runtime \
                    library was not found."
      ) else (
        (* We want to install locally. *)
        find_ocamlpicdir_no_opam ()
      )
    ) else (
      (* No opam. *)
      find_ocamlpicdir_no_opam ()
    )

  with Unix.Unix_error _ ->
    (* No opam. *)
    find_ocamlpicdir_no_opam ()


let ocamlpicdir =
  if not (Sys.file_exists @@ Vars.ocaml_dist ^ "/_install/lib/ocaml/libasmrun_pic.a") then
    find_ocamlpicdir ()
  else
    find_ocamlpicdir_no_opam ()


let atomise = List.map (fun a -> A a)

let cxxflags = Sh("`" ^ llvm_config ^
                  " --cxxflags | " ^
                  "sed 's/\-Wno\-maybe\-uninitialized/\-Wno\-uninitialized/g' |" ^
                  "sed 's/-fno-rtti//g'`"
                 ) :: atomise
  ("-Wall" ::
   "-Wextra" ::
   "-Werror" ::
   "-Wno-unused-parameter" ::
   "-std=c++11" ::
   "-pedantic" ::
   "-fcolor-diagnostics" ::

   "-fPIC" ::
   "-fexceptions" ::
   "-fvisibility=hidden" ::
   "-ggdb3" ::
   "-O0" ::

   ("-I" ^ ocamlpicdir) ::

   "-Itools/bridgen/c++" ::
   "-Iplugin/c++" ::
   "-D__STDC_CONSTANT_MACROS" ::
   "-D__STDC_LIMIT_MACROS" ::

   "-UNDEBUG" ::
   (match get_os_type () with
   | Linux -> []
   (* find where are boost headers installed by brew *)
   | OSX -> ["-I" ^ (read_stdout
                       "brew list boost | grep include | tail -1 | \
                        sed 's/include.*/include/g'")]))

let ldflags = Sh("`" ^ llvm_config ^ " --ldflags`") :: atomise
  ("-shared" ::
   "-lclangStaticAnalyzerCore" ::
   "-lclangAnalysis" ::
   "-lclangAST" ::
   "-lclangLex" ::
   "-lclangBasic" ::
   "-lLLVMSupport" ::
   "-lpthread" ::
   "-ldl" ::
   "-lncurses" ::
   "-lasmrun_pic" ::
   "-lunix" ::
   ("-L" ^ ocamlpicdir) ::
   ("-Wl,-rpath," ^ ocamlpicdir) ::
   "-ggdb3" ::
   [match get_os_type () with
   | Linux -> "-Wl,-z,defs"
   | OSX -> "-Wl,-no_compact_unwind"])

let headers = [
  "tools/bridgen/c++/ocaml++.h";
  "tools/bridgen/c++/ocaml++/bridge.h";
  "tools/bridgen/c++/ocaml++/private.h";
  "plugin/c++/OCamlVisitor/OCamlVisitor.h";
  "plugin/c++/OCamlChecker.h";
  "plugin/c++/ast_bridge.h";
  "plugin/c++/ast_bridge_of.h";
  "plugin/c++/backtrace.h";
  "plugin/c++/bridge_cache.h";
  "plugin/c++/clang_context.h";
  "plugin/c++/clang_enums.h";
  "plugin/c++/clang_ranges.h";
  "plugin/c++/clang_ref.h";
  "plugin/c++/clang_ref_holder.h";
  "plugin/c++/clang_type_traits.h";
  "plugin/c++/delayed_exit.h";
  "plugin/c++/dynamic_stack.h";
  "plugin/c++/heterogenous_container.h";
  "plugin/c++/sloc_bridge.h";
  "plugin/c++/trace.h";
  "plugin/c++/type_name.h";
]

let objects = [
  "tools/bridgen/c++/OCamlADT.o";
  "tools/bridgen/c++/value_of_context.o";
  "plugin/c++/OCamlVisitor/OCamlVisitor.o";
  "plugin/c++/OCamlVisitor/Decl.o";
  "plugin/c++/OCamlVisitor/Expr.o";
  "plugin/c++/OCamlVisitor/Stmt.o";
  "plugin/c++/OCamlVisitor/Type.o";
  "plugin/c++/OCamlVisitor/TypeLoc.o";
  "plugin/c++/OCamlChecker.o";
  "plugin/c++/PluginRegistration.o";
  "plugin/c++/ast_bridge.o";
  "plugin/c++/ast_bridge_of.o";
  "plugin/c++/backtrace.o";
  "plugin/c++/bridge_cache.o";
  "plugin/c++/clang_context.o";
  "plugin/c++/clang_enums.o";
  "plugin/c++/clang_operations.o";
  "plugin/c++/clang_ref.o";
  "plugin/c++/clang_ref_holder.o";
  "plugin/c++/delayed_exit.o";
  "plugin/c++/dynamic_stack.o";
  "plugin/c++/heterogenous_container.o";
  "plugin/c++/sloc_bridge.o";
  "plugin/c++/trace.o";
  "plugin/c++/type_name.o";
  "plugin/ocaml/clangLib.o";
]


let () =
  dispatch begin function
    | Before_options ->
        (*Options.ocaml_cflags := ["-warn-error"; "A"]*)
        Options.ocaml_cflags := ["-I"; "+camlp4/Camlp4Parsers"];
        Options.use_ocamlfind := true;

        flag ["c++"; "compile"; "no-rtti"] & A"-fno-rtti";

        pflag ["ocaml"; "link"; "byte"] "apron-link"
          (fun x -> Sh("`ocamlfind query apron`/" ^ x ^ ".cma"));
        pflag ["ocaml"; "link"; "native"] "apron-link"
          (fun x -> Sh("`ocamlfind query apron`/" ^ x ^ ".cmxa"));


    | After_rules ->
        rule "Generate map and fold visitors from bridge AST"
          ~prods:[
            "clang/clang/mapVisitor.ml";
            "clang/clang/foldVisitor.ml";
            "clang/clang/iterVisitor.ml";
          ]
          ~deps:[
            "tools/visitgen/visitgen.native";
            "clang/clang/astBridge.ml";
          ]
          begin fun env build ->
            Cmd (S[
              A"tools/visitgen/visitgen.native";
              A"clang/clang";
              A"clang/clang/astBridge.ml";
            ])
          end;


        rule "Generate simplified AST without sloc/cref/..."
          ~prods:[
            "clang/clang/astSimple.ml";
            "clang/clang/astSimplify.ml";
          ]
          ~deps:[
            "tools/simplify/simplify.native";
            "clang/clang/astBridge.ml";
          ]
          begin fun env build ->
            Cmd (S[
              A"tools/simplify/simplify.native";
              A"clang/clang";
              A"ast";
            ])
          end;


        rule "Generate file containing the search path for clangml.dylib"
          ~prod:"clang/clang/config.ml"
          begin fun env build ->
            Cmd (S[
              A"ocamlfind"; A"printconf"; A"destdir";
              Sh"|";
              A"sed"; A"-e"; A"s/.*/let destdir = \"&\"/";
              Sh">";
              A"clang/clang/config.ml";
            ])
          end;

        rule "Produce clean bridge AST without camlp4 extensions"
          ~prod:"clang/clang/%Bridge.ml"
          ~dep:"clang/clang/%.ml"
          begin fun env build ->
            Cmd (S[Sh(
                Printf.sprintf
                  "sed 's/deriving (Show)//g;s/= .*Bridge.* =/=/g' %s > %s"
                  (env "clang/clang/%.ml") (* input file *)
                  (env "clang/clang/%Bridge.ml")) (* ouput file *)
            ])
          end;

        rule "Bridge AST generation"
          ~prods:[
            "plugin/c++/%_bridge.cpp";
            "plugin/c++/%_bridge.h";
          ]
          ~deps:[
            "clang/clang/%Bridge.ml";
            "tools/bridgen/bridgen.native";
            (* This one is not a real dependency, but it makes sure
               that the target path exists. *)
            "plugin/c++/clang_ref.h";
          ]
          begin fun env build ->
            Cmd (S[
              A"tools/bridgen/bridgen.native";
              A"plugin/c++";
              A(env "%_bridge");
              A(env "clang/clang/%Bridge.ml")
            ])
          end;

        rule "C++ compilation"
          ~prod:"%.o"
          ~deps:("%.cpp" :: headers)
          begin fun env build ->
            let tags = tags_of_pathname (env "%.cpp") ++ "c++" ++ "compile" in

            Cmd (S(atomise [
              cpp_compiler;
              "-c"; "-o"; env "%.o";
            ] @ cxxflags @ [T tags; A(env "%.cpp")]))
          end;

        rule "OCaml library to object file"
          ~prod:"%.o"
          ~deps:[
            "%.cmxa";
            "%.a";
            "util.cmx";
            "util.o";
            "clang/clang.cmx";
            "clang/clang.o";
          ]
          begin fun env build ->
            Cmd (S (atomise [
              "ocamlfind";
              "ocamlopt";
              "util.cmx";
              "clang/clang.cmx";
              env "%.cmxa";
              "-output-obj";
              "-o";
              env "%.o";
              "-linkall";
              "-package"; "deriving";
              "-package"; "unix";
              "-package"; "dolog";
              "-linkpkg";
            ]))
          end;

        rule "Clang plugin"
          ~prod:"clangml.dylib"
          ~deps:objects
          begin fun env build ->
            Cmd (S(atomise ([
              cpp_compiler;
              "-o";
              "clangml.dylib" ;
            ] @ objects) @ ldflags))
          end;

    | _ -> ()
  end
