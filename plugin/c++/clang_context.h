#ifndef CLANG_CONTEXT_H
#define CLANG_CONTEXT_H

#include "clang_ref_holder.h"
#include "bridge_cache.h"

namespace clang
{
  struct SourceManager;
}

struct clang_context
{
  clang::SourceManager &SM;
  clang_ref_holder refs;
  bridge_cache cache;
};

#endif /* CLANG_CONTEXT_H */
