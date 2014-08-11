void f () {

  id lock;

  int i = 0;

  @synchronized (lock) 
    {
      i = 1;
      i = 2;
    }
  i = 3;
}
