#include "trace.h"

#include <cstdio>

unsigned int tracer::level;


tracer::tracer (char const *func, bool timing)
  : func (func)
  , timing (timing)
{
  std::printf ("%*s> %s\n", level, "", func);
  level += 2;
  gettimeofday (&start, NULL);
}


tracer::~tracer ()
{
  timeval end;
  gettimeofday (&end, NULL);

  timeval diff;
  timersub (&end, &start, &diff);

  level -= 2;
  std::printf ("%*s< %s", level, "", func);
  if (timing)
    std::printf (" (%ld.%06d)", diff.tv_sec, diff.tv_usec);
  std::fputc ('\n', stdout);
  std::fflush (stdout);
}
