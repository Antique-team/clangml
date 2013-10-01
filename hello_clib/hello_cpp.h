 

class Expr {

};

class UnitExpr : public Expr {
public:
	UnitExpr();
};

class IntConstExpr : public Expr {
public:
	IntConstExpr(int i);

private:
	int value;
};

class BinaryOpExpr : public Expr {
public:

	enum Op {
		Addition,
		Multiplication
	};

	BinaryOpExpr(Op op, Expr *e1, Expr *e2);

private:
	Op op;
	Expr *e1;
	Expr *e2;
};

class Stmt {


};

class SkipStmt : public Stmt {


};

class PrintStmt : public Stmt {
public:
	PrintStmt(Expr *e);
private:
	Expr *e;
};
