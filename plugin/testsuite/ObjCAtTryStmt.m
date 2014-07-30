void bar() {

  int i;

  @try {
    i = 1;
  }
  @catch (id anException) { 
    i = 2;
  }
  @finally {
    i = 3;
  }

  @try {
    i = 1;
  }
  @catch (id anException) { 
    i = 2;
  }
}
