#include "hello_cpp.h"

#include <stdio.h>


extern "C" {
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
}

extern "C" {
	CAMLprim value create_stmt();
}

extern "C" {
    void foo() {
        CAMLlocal1(iv);
    }
}

CAMLprim value
create_stmt() {
    CAMLparam0();
    
    printf("Creating stmt\n");

    Stmt *stmt = new PrintStmt(
                    new BinaryOpExpr(BinaryOpExpr::Addition, 
                        new BinaryOpExpr(BinaryOpExpr::Multiplication, 
                            new IntConstExpr(17), 
                            new IntConstExpr(9)), 
                        new IntConstExpr(43)));


    //stmt = new SkipStmt();
    
    CAMLreturn(stmt->ToValue());
}

int const Expr::expr_constructor_tag_sizes[];

CAMLprim value
UnitExpr::ToValue() {
    return Val_unit;
}

IntConstExpr::IntConstExpr(int i) {
	this->i = i;
}

CAMLprim
value IntConstExpr::ToValue() {
    CAMLparam0();
    CAMLlocal1(expr_value);
    
    expr_value = caml_alloc(expr_constructor_tag_sizes[IntConstTag], IntConstTag);
    
    Store_field(expr_value, 0, Val_int(i));
    
    CAMLreturn (expr_value);
}

BinaryOpExpr::BinaryOpExpr(Op op, Expr *e1, Expr *e2) {
	this->op = op;
	this->e1 = e1;
	this->e2 = e2;

}
CAMLprim
value BinaryOpExpr::ToValue() {
    CAMLparam0();
    CAMLlocal3(e1_value, e2_value, result_value);
    
    e1_value = e1->ToValue();
    
    e2_value = e2->ToValue();

    printf("BinaryOpTag is %d\n", BinaryOpTag);

    result_value = caml_alloc(expr_constructor_tag_sizes[BinaryOpTag], BinaryOpTag);
    
    Store_field(result_value, 0, Val_int(OpToTag(op)));
    
    Store_field(result_value, 1, e1_value);
    Store_field(result_value, 2, e2_value);
    
    printf("result_value tag is %d\n", Tag_val(result_value));
    CAMLreturn(result_value);
}

 binary_op_constant_tag BinaryOpExpr::OpToTag(Op op) {
    switch (op) {
        case Addition:
            return BinaryOp_AddTag;
            
        case Multiplication:
            return BinaryOp_MultiplyTag;
    }
}


int const Stmt::stmt_constructor_tag_sizes[];

CAMLprim
value SkipStmt::ToValue() {
    CAMLparam0();
    
    CAMLreturn(Val_int(SkipTag));
}

PrintStmt::PrintStmt(Expr *e) {
	this->e = e;
}

CAMLprim
value PrintStmt::ToValue() {
    CAMLparam0();
    CAMLlocal2(expression_value, result_value);
    
    expression_value = e->ToValue();

    result_value = caml_alloc(stmt_constructor_tag_sizes[PrintTag], PrintTag);
    Store_field(result_value, 0, expression_value);
    
    CAMLreturn(result_value);
}
