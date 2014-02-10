#ifndef TRACE_H
#define TRACE_H

#include <cstdio>
#include <sys/time.h>


static unsigned int level = 0;
struct tracer
{
  tracer (char const *func, bool timing)
    : func (func)
    , timing (timing)
  {
    std::printf ("%*s> %s\n", level++, "", func);
    gettimeofday (&start, NULL);
  }

  ~tracer ()
  {
    timeval end;
    gettimeofday (&end, NULL);

    timeval diff;
    timersub (&end, &start, &diff);

    std::printf ("%*s< %s", --level, "", func);
    if (timing)
      std::printf (" (%ld.%06ld)", diff.tv_sec, diff.tv_usec);
    std::fputc ('\n', stdout);
    std::fflush (stdout);
  }

  char const *func;
  bool timing;
  timeval start;
};

#define TRACE tracer trace (__func__, false)
#define TIME tracer trace (__func__, true)

#endif /* TRACE_H */
