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
    int result = a.getTypePtr ()
               - b.getTypePtr ();
    if (result == 0)
      result = a.getLocalQualifiers ().getAsOpaqueValue ()
             - b.getLocalQualifiers ().getAsOpaqueValue ();
    return result < 0;
  }

  bool
  operator < (TypeLoc a, TypeLoc b)
  {
    // This ignores the source locations.
    return a.getType () < b.getType ();
  }
}
