
void f () {

  int b;
  int a = 0;
  b = a++;
  // should become
  /*
  int b;
  int a;
  a = 0;
  int tmp0;
  tmp0 = a;
  a = a + 1;
  b = tmp0;
  */
  b = a--;
  // should become
  /*
  int tmp1;
  tmp1 = a;
  a = a - 1;
  b = tmp1;
  */
}
