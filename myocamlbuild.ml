open Ocamlbuild_plugin

let ocamldir = Sys.getenv "HOME" ^ "/.opam/4.01.0+PIC"

let atomise = List.map (fun a -> A a)

let cxxflags = Sh"`llvm-config-3.4 --cxxflags`" :: atomise [
  "-Wall";
  "-Wextra";
  "-Werror";
  "-Wno-unused-parameter";
  "-std=c++11";
  "-pedantic";
  "-fcolor-diagnostics";

  "-fPIC";
  "-fexceptions";
  "-fvisibility=hidden";
  "-ggdb3";
  "-O0";

  "-Itools/bridgen/c++";
  "-Iplugin/c++";
  "-D__STDC_CONSTANT_MACROS";
  "-D__STDC_LIMIT_MACROS";

  "-UNDEBUG";
]

let ldflags = Sh"`llvm-config-3.4 --ldflags`" :: atomise [
  "-Wl,-z,defs";
  "-shared";
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
  "tools/bridgen/c++/ocaml++.o";
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

        rule "Generate file containing the search path for clangaml.dylib"
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
            Cmd (S[
              A"grep"; A"-v"; A"^\\s*deriving (Show)\\s*$";
              A(env "clang/clang/%.ml");
              Sh"|";
              A"sed"; A"-e";
              A("s/\\s\\+deriving (Show)//;s/= "
                ^ String.capitalize (env "%")
                ^ "Bridge.\\w\\+ //");
              Sh">";
              A(env "clang/clang/%Bridge.ml");
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

        flag ["compile"; "c++"; "no-rtti"] (A"-fno-rtti");

        rule "C++ compilation"
          ~prod:"%.o"
          ~deps:("%.cpp" :: headers)
          begin fun env build ->
            let tags = tags_of_pathname (env "%.cpp") ++ "compile" ++ "c++" in

            Cmd (S(atomise [
              "clang++";
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
              "-package"; "unix";
              "-package"; "deriving-ocsigen";
              "-linkpkg";
            ]))
          end;

        rule "Clang plugin"
          ~prod:"clangaml.dylib"
          ~deps:objects
          begin fun env build ->
            Cmd (S(atomise ([
              "clang++";
              "-o";
              "clangaml.dylib" ;
            ] @ objects) @ ldflags))
          end;

        rule "Noweb to OCaml interface file"
          ~prod:"%.mli"
          ~dep:"%.ml.nw"
          begin fun env build ->
            Cmd (S[
              A"notangle";
              A"-L# %L \"%F\"%N";
              A"-R.mli";
              A(env "%.ml.nw");
              Sh">";
              A(env "%.mli");
            ])
          end;

        rule "Noweb to OCaml implementation file"
          ~prod:"%.ml"
          ~dep:"%.ml.nw"
          begin fun env build ->
            Cmd (S[
              A"notangle";
              A"-L# %L \"%F\"%N";
              A"-R.ml";
              A(env "%.ml.nw");
              Sh">";
              A(env "%.ml");
            ])
          end;

    | _ ->
        ()
  end
