struct astruct
{
  int myarray[20][1][1];
};

struct derived
  : astruct
{
};

template<typename T>
struct parent
{
  derived d;

  int i = __builtin_offsetof (T, myarray);
};

int i = __builtin_offsetof (parent<derived>, d.myarray[0][1][1]);
