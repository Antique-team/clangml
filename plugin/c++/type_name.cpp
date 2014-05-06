#include "type_name.h"

#include <cxxabi.h>

std::string
demangle (std::type_info const &ti)
{
  size_t length;
  int status;
  if (char *name = __cxxabiv1::__cxa_demangle (ti.name (), NULL, &length, &status))
    {
      std::string result (name);
      free (name);
      return result;
    }
#if 0
  switch (status)
    {
    case -1: return "A memory allocation failure occurred.";
    case -2: return "Type name is not a valid name under the C++ ABI mangling rules.";
    case -3: return "One of the arguments is invalid.";
    }
#endif
  return ti.name ();
}
