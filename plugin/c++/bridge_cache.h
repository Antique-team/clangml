#include "bridge_ast.h"
#include "heterogenous_container.h"

#include <clang/AST/Type.h>
#include <clang/AST/TypeLoc.h>

#include <map>

namespace clang
{
  bool operator < (Qualifiers a, Qualifiers b);
  bool operator < (QualType a, QualType b);
  bool operator < (TypeLoc a, TypeLoc b);
}


template<typename T>
struct concrete_bridge_cache
  : heterogenous_container<concrete_bridge_cache>::concrete<T>
{
  std::map<T, adt_ptr> cache;

  adt_ptr get (T p)
  {
    auto found = cache.find (p);
    if (found == cache.end ())
      return nullptr;
    return found->second;
  }

  void put (T p, adt_ptr value)
  {
    cache.insert (std::make_pair (p, value));
  }
};


struct bridge_cache
  : heterogenous_container<concrete_bridge_cache>
{
  template<typename T>
  adt_ptr cached_value (T p, adt_ptr value)
  {
    if (value)
      {
        get_concrete<T> ().put (p, value);
        return value;
      }
    return get_concrete<T> ().get (p);
  }
};
