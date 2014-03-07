#ifndef CLANG_REF_H
#define CLANG_REF_H

#include "ocaml++.h"

template<typename Node>
struct clang_ref
{
  size_t id;
};


template<typename Node>
static inline value
value_of (value_of_context &ctx, clang_ref<Node> ref)
{
  return value_of (ctx, ref.id);
}

#endif /* CLANG_REF_H */
