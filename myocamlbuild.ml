open Ocamlbuild_plugin

let clangdir = "plugin/ext/clang/build/Debug+Asserts"
let ocamldir = Sys.getenv "HOME" ^ "/.opam/4.01.0+PIC"

let atomise = List.map (fun a -> A a)

let cxxflags = atomise [
  "-Itools/bridgen/c++";
  "-Iplugin/c++";
  "-I../" ^ clangdir ^ "/include";
  "-Icamlp4_autogen/cutil";
  "-D__STDC_CONSTANT_MACROS";
  "-D__STDC_LIMIT_MACROS";
  "-fPIC";
  "-fvisibility=hidden";
  "-std=c++11";
  "-ggdb3";
  "-O3";
]

let ldflags = [
  "-Wl,-z,defs";
  "-shared";
  "-L../" ^ clangdir ^ "/lib";
  "-lclangStaticAnalyzerCore";
  "-lclangAnalysis";
  "-lclangAST";
  "-lclangLex";
  "-lclangBasic";
  "-lLLVMSupport";
  "-lpthread";
  "-ldl";
  "-ltinfo";
  "-lasmrun";
  "-lunix";
  "-L" ^ ocamldir ^ "/lib/ocaml";
  "-Wl,-rpath," ^ ocamldir ^ "/lib/ocaml";
  "-ggdb3";
]

let headers = [
  "tools/bridgen/c++/ocaml++.h";
  "plugin/c++/OCamlVisitor/OCamlVisitor.h";
  "plugin/c++/OCamlVisitor/operators.h";
  "plugin/c++/OCamlChecker.h";
  "plugin/c++/bridge_ast.h";
  "plugin/c++/bridge_ast_of.h";
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
  "plugin/c++/trace.h";
]

let objects = [
  "tools/bridgen/c++/ocaml++.o";
  "plugin/c++/OCamlVisitor/OCamlVisitor.o";
  "plugin/c++/OCamlVisitor/Decl.o";
  "plugin/c++/OCamlVisitor/Expr.o";
  "plugin/c++/OCamlVisitor/Stmt.o";
  "plugin/c++/OCamlVisitor/Type.o";
  "plugin/c++/OCamlVisitor/TypeLoc.o";
  "plugin/c++/OCamlChecker.o";
  "plugin/c++/PluginRegistration.o";
  "plugin/c++/bridge_ast.o";
  "plugin/c++/bridge_ast_of.o";
  "plugin/c++/bridge_cache.o";
  "plugin/c++/clang_context.o";
  "plugin/c++/clang_enums.o";
  "plugin/c++/clang_operations.o";
  "plugin/c++/clang_ref.o";
  "plugin/c++/clang_ref_holder.o";
  "plugin/c++/delayed_exit.o";
  "plugin/c++/dynamic_stack.o";
  "plugin/c++/heterogenous_container.o";
  "plugin/c++/trace.o";
  "plugin/ocaml/clangLib.o";
]


let () =
  dispatch begin function
    | Before_options ->
        (*Options.ocaml_cflags := ["-warn-error"; "A"]*)
        Options.ocaml_cflags := ["-I"; "+camlp4/Camlp4Parsers"]

    | After_rules ->
        rule "Generate map and fold visitors from bridge AST"
          ~prods:[
            "clang/clang/visitor/mapVisitor.ml";
            "clang/clang/visitor/foldVisitor.ml"
          ]
          ~deps:[
            "clang/clang/bridge.ml";
            "tools/visitgen/visitgen.native";
            (* This one is not a real dependency, but it makes sure
               that the target path exists. *)
            "clang/clang/visitor/visitor.ml";
          ]
          begin fun env build ->
            Cmd (S[
              A"tools/visitgen/visitgen.native";
              A"clang/clang/visitor";
              A"clang/clang/bridge.ml";
            ])
          end;

        rule "Produce clean bridge AST without camlp4 extensions"
          ~prod:"clang/clang/bridge.ml"
          ~dep:"clang/clang/ast.ml"
          begin fun env build ->
            Cmd (S[
              A"grep"; A"-v"; A"^\\s*deriving (Show)\\s*$";
              A"clang/clang/ast.ml";
              Sh"|";
              A"sed"; A"-e";
              A"s/\\s\\+deriving (Show)//;s/= Bridge.\\w\\+ //";
              Sh">";
              A"clang/clang/bridge.ml";
            ])
          end;

        rule "Bridge AST generation"
          ~prods:["plugin/c++/bridge_ast.cpp"; "plugin/c++/bridge_ast.h"]
          ~deps:[
            "clang/clang/bridge.ml";
            "tools/bridgen/bridgen.native";
            (* This one is not a real dependency, but it makes sure
               that the target path exists. *)
            "plugin/c++/clang_ref.h";
          ]
          begin fun env build ->
            Cmd (S[A"tools/bridgen/bridgen.native"; A"plugin/c++"; A"bridge_ast"; A"clang/clang/bridge.ml"])
          end;

        flag ["compile"; "c++"; "no-rtti"] (A"-fno-rtti");

        rule "C++ compilation"
          ~prod:"%.o"
          ~deps:("%.cpp" :: headers)
          begin fun env build ->
            let tags = tags_of_pathname (env "%.cpp") ++ "compile" ++ "c++" in

            Cmd (S([
              A("../" ^ clangdir ^ "/bin/clang++");
              A"-c"; A"-o"; A(env "%.o");
            ] @ cxxflags @ [T tags; A(env "%.cpp")]))
          end;

        rule "OCaml library to object file"
          ~prod:"%.o"
          ~deps:[
            "%.cmxa";
            "%.a";
            "util/formatx.cmx";
            "util/logger.cmx";
            "clang/clang.cmx";
            "util/formatx.o";
            "util/logger.o";
            "clang/clang.o";
          ]
          begin fun env build ->
            Cmd (S (atomise [
              "ocamlfind";
              "ocamlopt";
              "util/formatx.cmx";
              "util/logger.cmx";
              "clang/clang.cmx";
              env "%.cmxa";
              "-output-obj";
              "-o";
              env "%.o";
              "-linkall";
              "-package"; "unix";
              "-package"; "deriving-ocsigen";
              "-linkpkg";
            ]))
          end;

        rule "Clang plugin"
          ~prod:"clangaml.dylib"
          ~deps:objects
          begin fun env build ->
            Cmd (S(atomise @@ [
              "../" ^ clangdir ^ "/bin/clang++";
              "-o";
              "clangaml.dylib" ;
            ] @ objects @ ldflags))
          end;

    | _ ->
        ()
  end
