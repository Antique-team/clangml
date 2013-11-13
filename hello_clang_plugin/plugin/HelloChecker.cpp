//
//  HelloChecker.cpp
//  HelloClangPlugin
//
//  Created by Devin Coughlin on 10/16/13.
//  Copyright (c) 2013 Devin Coughlin. All rights reserved.
//

#include <caml/callback.h>

#include "HelloChecker.h"

#include <llvm/Support/raw_ostream.h>

extern "C" {
    CAMLprim value
    caml_print_hello(value unit);
}

using namespace clang;
using namespace ento;

void hello_closure() {
    static value * closure_f = NULL;
    if (closure_f == NULL) {
        closure_f = caml_named_value("Hello callback");
    }
    caml_callback(*closure_f, Val_unit);
}

CAMLprim value
caml_print_hello(value unit)
{
    printf("Hello from C\n");
    return Val_unit;
}

void initialize_caml() {
	// Make sure caml main is called once and only once
	
	static bool already_initialized = false;
	
	if (!already_initialized) {
        
        // Create fake argv on heap
        // We leak this, but since we do not know what ocaml
        // is doing with this array, it is not safe to stack-allocated
        // or free after we are done.
		char **argv = (char **)malloc(sizeof(char *)*1);
    	argv[0] = NULL;
    
    	caml_main(argv);
        already_initialized =  true;
	}
}

void HelloChecker::checkASTDecl	( const	TranslationUnitDecl * 	D, AnalysisManager & 	Mgr, BugReporter & 	BR ) const {
    llvm::outs() << "Running Hello Checker on translation unit!" << "\n";
    
    initialize_caml();
    clang::DeclContext::decl_iterator current;
    current = D->decls_begin();
    for(current = D->decls_begin(); current != D->decls_end(); current++){
        //FunctionDecl * n = dynamic_cast<FunctionDecl*>(*current);
        Decl *c = *current;
        FunctionDecl * fCastTry = dyn_cast<FunctionDecl>(c);
        printf("fCastTry: %p\n",fCastTry);
        if(fCastTry){
            //Get main function
            if(fCastTry->isMain()){
                fCastTry->dump();
                Stmt *mainBody = fCastTry->getBody();
                mainBody->dump();
                
            }
            //TODO: extract statement tree
            //TODO: pass into ocaml
        }
    }
    hello_closure();
}