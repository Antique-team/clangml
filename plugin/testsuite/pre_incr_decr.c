
void f () {

  int a = 0;

  ++a;
  // should become
  a = a + 1;

  --a;
  // should become
  a = a - 1;
}
