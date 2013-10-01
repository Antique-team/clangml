#include "hello_cpp.h"

#include <stdio.h>

#include <caml/mlvalues.h>


extern "C" {
	CAMLprim value
	create_stmt();
}


CAMLprim value
create_stmt() {
    printf("Creating stmt\n");

    Stmt *stmt = new PrintStmt(new BinaryOpExpr(BinaryOpExpr::Addition, new IntConstExpr(1), new IntConstExpr(2)));
    

    return Val_unit;
}


IntConstExpr::IntConstExpr(int i) {
	this->value = i;
}

BinaryOpExpr::BinaryOpExpr(Op op, Expr *e1, Expr *e2) {
	this->op = op;
	this->e1 = e1;
	this->e2 = e2;
}

PrintStmt::PrintStmt(Expr *e) {
	this->e = e;
}