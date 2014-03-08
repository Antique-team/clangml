#ifndef CLANG_REF_H
#define CLANG_REF_H

#include "ocaml++.h"

struct clang_ref_base
{
  size_t id;

  clang_ref_base ();
  clang_ref_base (size_t id);
};

template<typename Node>
struct clang_ref
  : clang_ref_base
{
  using clang_ref_base::clang_ref_base;
};


template<typename Node>
static inline value
value_of (value_of_context &ctx, clang_ref<Node> ref)
{
  return value_of (ctx, ref.id);
}

#endif /* CLANG_REF_H */
