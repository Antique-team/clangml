int
foo (int arg)
{
  int a = 1, b;
  b = 1;

  int *p = &a;
  int *q = &b;

  while (b > 10 ? 1 : b - 1)
    *(a ? p : q) = (arg ? 0 : 1) ? (arg > 3 ? 2 : 3) : b;

  for (a = 1; a < 3; a = a + 1)
    b = a;

  switch (a)
    {
    case 3:
      return 1;
    case -2:
    case -1:
    case 0:
    case 4 ... 7:
      return 2;
    case 2:
    default:
      return 3;
    }

  return 0;
}

int
main ()
{
  foo (0);
}
