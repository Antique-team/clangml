
void f() {
#define P(n,args) p(#n #args, __builtin_##n args)
  P(types_compatible_p, (int, float));
}
