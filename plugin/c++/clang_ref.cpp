#include "clang_ref.h"

clang_ref_base::clang_ref_base ()
  : id (-1u)
{
}


clang_ref_base::clang_ref_base (size_t id)
  : id (id)
{
  if (id == 0)
    throw std::runtime_error ("null reference passed to clang API");
  if (id == -1u)
    throw std::runtime_error ("invalid reference passed to clang API");
}
