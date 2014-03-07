open Ocamlbuild_plugin

let ext = "plugin/ext/clang/build/Debug+Asserts"
let ocamldir = Sys.getenv "HOME" ^ "/.opam/4.01.0+PIC"

let atomise = List.map (fun a -> A a)

let cxxflags = atomise [
  "-Ibridgen/c++";
  "-I../" ^ ext ^ "/include";
  "-Icamlp4_autogen/cutil";
  "-D__STDC_CONSTANT_MACROS";
  "-D__STDC_LIMIT_MACROS";
  "-fPIC";
  "-std=c++11";
  "-ggdb3";
  "-O3";
]

let ldflags = atomise [
  "-Wl,-z,defs";
  "-shared";
  "-L../" ^ ext ^ "/lib";
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
  "bridgen/c++/ocaml++.h";
  "plugin/c++/bridge_ast.h";
  "plugin/c++/bridge_cache.h";
  "plugin/c++/clang_context.h";
  "plugin/c++/clang_hacks.h";
  "plugin/c++/clang_ref.h";
  "plugin/c++/clang_ref_holder.h";
  "plugin/c++/clang_type_traits.h";
  "plugin/c++/dynamic_stack.h";
  "plugin/c++/enums.h";
  "plugin/c++/HelloChecker.h";
  "plugin/c++/heterogenous_container.h";
  "plugin/c++/OCamlVisitor.h";
  "plugin/c++/trace.h";
]

let objects = [
  "bridgen/c++/ocaml++.o";
  "plugin/c++/HelloChecker.o";
  "plugin/c++/OCamlVisitor.o";
  "plugin/c++/PluginRegistration.o";
  "plugin/c++/bridge_ast.o";
  "plugin/c++/bridge_cache.o";
  "plugin/c++/clang_context.o";
  "plugin/c++/clang_operations.o";
  "plugin/c++/clang_ref_holder.o";
  "plugin/c++/heterogenous_container.o";
  "plugin/c++/trace.o";
  "plugin/ocaml/clangLib.o";
]


let () =
  dispatch begin function
    | After_rules ->
        rule "Produce clean bridge AST without camlp4 extensions"
          ~prod:"clang/bridge.ml"
          ~dep:"clang/ast.ml"
          begin fun env build ->
            Cmd (S[
              A"grep"; A"-v"; A"^\\s*deriving (Show)\\s*$";
              A"clang/ast.ml";
              Sh"|";
              A"sed"; A"-e";
              A"s/\\s\\+deriving (Show)//;s/= Bridge.\\w\\+ //";
              Sh">";
              A"clang/bridge.ml";
            ])
          end;

        rule "Bridge AST generation"
          ~prods:["plugin/c++/bridge_ast.cpp"; "plugin/c++/bridge_ast.h"]
          ~deps:["clang/bridge.ml"; "bridgen/bridgen.native"]
          begin fun env build ->
            Cmd (S[A"bridgen/bridgen.native"; A"plugin/c++"; A"bridge_ast"; A"clang/bridge.ml"])
          end;

        rule "C++ compilation"
          ~prod:"%.o"
          ~deps:("%.cpp" :: headers)
          begin fun env build ->
            Cmd (S([
              A("../" ^ ext ^ "/bin/clang++");
              A"-c"; A"-o"; A(env "%.o");
            ] @ cxxflags @ [A(env "%.cpp")]))
          end;

        rule "No-RTTI C++ compilation"
          ~prod:"%.o"
          ~deps:("%.no-rtti.cpp" :: headers)
          begin fun env build ->
            Cmd (S([
              A("../" ^ ext ^ "/bin/clang++");
              A"-c"; A"-o"; A(env "%.o");
            ] @ cxxflags @ [A"-fno-rtti"; A(env "%.cpp")]))
          end;

        rule "OCaml library to object file"
          ~prod:"%.o"
          ~deps:["%.cmxa"; "%.a"]
          begin fun env build ->
            Cmd (S (atomise [
              "ocamlfind";
              "ocamlopt";
              "util/formatx.cmx";
              "util/logger.cmx";
              "clang.cmx";
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
            Cmd (S([
              A("../" ^ ext ^ "/bin/clang++");
              A"-o";
              A"clangaml.dylib" ;
            ] @ atomise objects @ ldflags))
          end;

    | _ ->
        ()
  end
