#include "delayed_exit.h"

#include <cstdlib>

delayed_exit::~delayed_exit ()
{
  exit (code);
}
