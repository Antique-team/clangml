#include "bridge_cache.h"

namespace clang
{
  bool
  operator < (Qualifiers a, Qualifiers b)
  {
    return a == b ? 0 : b.isStrictSupersetOf (a);
  }

  bool
  operator < (QualType a, QualType b)
  {
    return a.getTypePtr () < b.getTypePtr ()
        || a.getLocalQualifiers () < b.getLocalQualifiers ();
  }

  bool
  operator < (TypeLoc a, TypeLoc b)
  {
    // This ignores the source locations.
    return a.getType () < b.getType ();
  }
}
