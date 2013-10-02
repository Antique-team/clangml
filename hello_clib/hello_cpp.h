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
        UnitTag
    };
    
    enum expr_block_tag {
        IntConstTag, // size = 1
        BinaryOpTag // size = 3
    };
    
    constexpr static int const expr_constructor_tag_sizes[] = {1, 3};
    
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
 */


class Stmt : public OCamlADTBase {
public:

protected:
    
    enum stmt_constant_tag {
        SkipTag
    };
    
    enum expr_block_tag {
        PrintTag // size = 1
    };
    
    constexpr static int const stmt_constructor_tag_sizes[] = {1};
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
