void f () {

  id lock;

  int i = 1;

  @synchronized (lock) 
    {
      i = 2;
    }
}
