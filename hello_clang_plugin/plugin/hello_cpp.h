#include <list>

#ifndef __HelloClangPlugin__hello_cpp__
#define __HelloClangPlugin__hello_cpp__
namespace hello_clang{
extern "C" {
#include <stdio.h>
#include <caml/mlvalues.h>
}
class OCamlADTBase {
public:
    
    
    virtual CAMLprim value ToValue() = 0;
};


/*
 type binary_op =
 | BinaryOp_Add
 | BinaryOp_Multiply
 */

enum binary_op_constant_tag {
    BinaryOp_AddTag,
    BinaryOp_MultiplyTag
};


class Expr : public OCamlADTBase {
public:
    
protected:
    enum expr_constant_tag {
        UnitTag = 0
    };
    
    enum expr_block_tag {
        IntConstTag = 0, // size = 1
        BinaryOpTag = 1// size = 3
    };
    
    static int const expr_constructor_tag_sizes[];
    
};

/*
 
 type expr =
 | Unit
 | IntConst of int
 | BinaryOp of binary_op * expr * expr
 
 */






class UnitExpr : public Expr {
public:
	UnitExpr();
    
    virtual CAMLprim value ToValue();
    
};

class IntConstExpr : public Expr {
public:
	IntConstExpr(int i);

    virtual CAMLprim value ToValue();
    
private:
	int i;
};

class BinaryOpExpr : public Expr {
public:

    enum Op {
        Addition,
        Multiplication
    };


	BinaryOpExpr(Op op, Expr *e1, Expr *e2);
    
    virtual CAMLprim value ToValue();

    static binary_op_constant_tag OpToTag(Op op);
    
private:
	Op op;
	Expr *e1;
	Expr *e2;
};



/*
 type stmt =
 | Skip
 | Print of expr
 | Block of stmt list
 */


class Stmt : public OCamlADTBase {
public:

protected:
    
    enum stmt_constant_tag {
        SkipTag
    };
    
    enum expr_block_tag {
        PrintTag, // size = 1
        BlockTag // size = 1
    };
    
    static int const stmt_constructor_tag_sizes[];
};

class SkipStmt : public Stmt {
public:
    
    virtual CAMLprim value ToValue();
    

};

class PrintStmt : public Stmt {
public:
	PrintStmt(Expr *e);
    
    virtual CAMLprim value ToValue();
private:
	Expr *e;
};



class BlockStmt : public Stmt {
public:
	BlockStmt(const std::list<Stmt *> &stmts);
    
    virtual CAMLprim value ToValue();
private:
    std::list<Stmt *> stmts;
};
}
#endif /* defined(__HelloClangPlugin__hello_cpp__) */