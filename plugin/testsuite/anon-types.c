typedef struct arith {
  union {
    int op;
  } u;
} arith ;

int foo () {
  arith a;

  a.u.op = 3;
}
