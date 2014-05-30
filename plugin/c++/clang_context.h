#ifndef CLANG_CONTEXT_H
#define CLANG_CONTEXT_H

#include "bridge_cache.h"
#include "clang_ref_holder.h"

namespace clang
{
  class ASTContext;
}

struct clang_context
{
  clang::ASTContext &Ctx;
  clang_ref_holder refs;
  bridge_cache cache;
  value_of_context values;

  clang_context (clang::ASTContext &Ctx)
    : Ctx (Ctx)
  {
  }

  clang::ASTContext *operator -> ()
  {
    return &Ctx;
  }
};

#endif /* CLANG_CONTEXT_H */
