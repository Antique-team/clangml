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

#include "hello_cpp.h"

#include <iostream>

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
hello_clang::Expr* HelloChecker::convertExpr(const clang::Expr * in) const {
    //Check for binary operator
    const BinaryOperator * binOp = dyn_cast<BinaryOperator>(in);
    if(binOp) {
        if(binOp->getOpcode() == BO_Add) {
            hello_clang::BinaryOpExpr::Op oper = hello_clang::BinaryOpExpr::Addition;
            return new hello_clang::BinaryOpExpr(oper, convertExpr(binOp->getLHS()), convertExpr(binOp->getRHS()));
        }
        if(binOp->getOpcode() == BO_Mul) {
            hello_clang::BinaryOpExpr::Op oper = hello_clang::BinaryOpExpr::Multiplication;
            return new hello_clang::BinaryOpExpr(oper, convertExpr(binOp->getLHS()), convertExpr(binOp->getRHS()));
        }
        return NULL;  //Return null if not defined
    }
    const IntegerLiteral * intLit = dyn_cast<IntegerLiteral>(in);
    if(intLit) {
        //TODO:
        //std::cout << intLit->getValue().getSExtValue() << "\n";
        int value = intLit->getValue().getSExtValue();
        return new hello_clang::IntConstExpr(value);
    }
    return NULL; //Return null if something other than int lit or binop is encountered
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
        //printf("fCastTry: %p\n",fCastTry);
        if(fCastTry){
            //Get main function
            if(fCastTry->isMain()){
                fCastTry->dump();
                clang::Stmt *mainBody = fCastTry->getBody();
                CompoundStmt * compoundStmt = dyn_cast<CompoundStmt>(mainBody);
                
                printf("main body dump\n");
                mainBody->dump();
                if(compoundStmt) {
                    clang::Stmt** currentSt;
//                    cmpStmtIterator = compoundStmt->body_begin();
                    for(currentSt = compoundStmt->body_begin(); currentSt != compoundStmt->body_end(); currentSt++){
                        ReturnStmt * returnStmt = dyn_cast<ReturnStmt>(*currentSt);
                        if(returnStmt){
                            clang::Expr * retVal = returnStmt->getRetValue();
                            retVal->dump();
                            convertExpr(retVal);
                        }

                    }
                }
                
                
            }
            //TODO: extract statement tree
            //TODO: pass into ocaml
        }
    }
    hello_closure();
}