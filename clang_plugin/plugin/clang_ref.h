#ifndef CLANG_REF_H
#define CLANG_REF_H

#include "ocaml++.h"

struct clang_ref
{
  int id;
};


static inline value
value_of (value_of_context &ctx, clang_ref ref)
{
  return value_of (ctx, ref.id);
}

#endif /* CLANG_REF_H */
