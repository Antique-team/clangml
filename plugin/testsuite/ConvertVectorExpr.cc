typedef double vector8double __attribute__((__vector_size__(64)));
typedef float  vector8float  __attribute__((__vector_size__(32)));

vector8float f(vector8double input) {
  return __builtin_convertvector(input, vector8float);
}
