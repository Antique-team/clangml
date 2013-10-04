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
    
    std::list<Stmt *> blockStmts;
    
    blockStmts.push_back(new PrintStmt(
                                       new BinaryOpExpr(BinaryOpExpr::Addition,
                                                        new BinaryOpExpr(BinaryOpExpr::Multiplication,
                                                                         new IntConstExpr(17),
                                                                         new IntConstExpr(9)),
                                                        new IntConstExpr(43))));
    
    blockStmts.push_back(new SkipStmt());
    
    for (int i = 0; i < 10; i++) {
        blockStmts.push_back(new PrintStmt(new IntConstExpr(i)));
    }
    
    BlockStmt *blockStmt = new BlockStmt(blockStmts);

    
    CAMLreturn(blockStmt->ToValue());
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

BlockStmt::BlockStmt(const std::list<Stmt *> &stmts) {
    this->stmts = stmts;
}


template <class T>
CAMLprim
value ToList(const T &begin, const T &end) {
    CAMLparam0();
    CAMLlocal3(start_value, cons_value, tmp_value);
    
    // It would be much easier to build the list backwards,
    // but we may not have that kind of iterator
    
    if (begin == end) {
        return Val_emptylist;
    }
    
    start_value = caml_alloc(2, 0);
    Store_field(start_value, 0, (*begin)->ToValue()); // head is first item
    
    cons_value = start_value;
    
    T i = begin;
    i++;
    for ( ; i != end; i++) {
        
        tmp_value = caml_alloc(2, 0); 
        Store_field(cons_value, 1, tmp_value); // tail is not yet fully constructed rest of list
        
        cons_value = tmp_value;
        Store_field(cons_value, 0, (*i)->ToValue()); //
        
    }
    
    Store_field(cons_value, 1, Val_emptylist); // tail of last cons is empty list
    
    CAMLreturn(start_value);
}

CAMLprim
value BlockStmt::ToValue() {
    CAMLparam0();
    CAMLlocal2(list_value, result_value);
    
    list_value = ToList(stmts.begin(), stmts.end());
    
    
    result_value = caml_alloc(stmt_constructor_tag_sizes[BlockTag], BlockTag);
    Store_field(result_value, 0, list_value);
    
    
    CAMLreturn(result_value);
}
