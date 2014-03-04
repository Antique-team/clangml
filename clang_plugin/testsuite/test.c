// no prototype decl
int foo ();
// no args decl
int foo (void);
// var decl
int a;

// incomplete array
char b[] = "hello";

// fun decl without names
int run (char const *, int, char **);

// fun decl with names
int run (char const *name, int argc, char **argv);

// fun defn
int run (char const *name, int argc, char **argv) {
  // local var decl
  int a, b;

  // if without else
  if ((a = 0))
    return 0;

  // if with else
  if (a == 0)
    return 0;
  else
    return 1;

  // expression statement
  a = 2;

  // for with expr-stmt
  for (a = 0; a < 1; a++)
    ;

  // for without init
  for (; a < 1; a++)
    ;

  // for without cond and with decl-stmt
  for (int a = 0; ; a++)
    ;

  // for without inc
  for (a = 0; a < 1;)
    continue;

  // infinite loop
  for (;;)
    break;

  // switch statement
  switch (0)
    {
    case -1: // nested case-stmt
    case 0: // single case
      ;
    case 1 ... 3: // range-case
      break;
    }

  // some unary and binary ops
  return -2+!3*~4;
}

void foobar () { return; }
