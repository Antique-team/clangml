typedef struct B *B_ptr;

typedef struct A
{
  B_ptr b;
} A;

typedef struct B
{
  struct A *a;
} B;
