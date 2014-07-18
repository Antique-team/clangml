void test1() {
  int i = 0;
  int j = 0;
  #pragma clang __debug captured
  {
    ++i;
    ++j;
  }
}
