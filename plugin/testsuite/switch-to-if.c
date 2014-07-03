void blah ()
{
}

void foo (int stuff, int cond, int anothercond)
{
  while (stuff)
    { // #1
      blah ();
      switch (cond)
        {
        case 0:
          if (anothercond)
            continue;
        case 1:
          1;
          break;
        case 2:
          2;
          break;
        }
    }


  // =?=>

  while (stuff)
    { // #1
      blah ();
      do // #2
        {
          int tmp = cond;
          if (tmp == 0)
            {
              if (anothercond)
                // XXX: continue jumps to #2, instead of #1
                continue;
            }
          if (tmp == 0 || tmp == 1)
            {
              1;
              break;
            }
          if (tmp == 0 || tmp == 1 || tmp == 2)
            {
              2;
              break;
            }
        }
      while (0);
    }
}
