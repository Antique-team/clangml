typedef struct A {
  struct {
    int i;
  } v;
} A;

int main () {
  A a;
  a.v.i = 0;
}
