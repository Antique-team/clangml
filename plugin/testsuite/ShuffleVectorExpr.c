#define __MM_MALLOC_H

#include <x86intrin.h>

unsigned int test_bzhi_u32(unsigned int __X, unsigned int __Y) {
  return _bzhi_u32(__X, __Y);
}
