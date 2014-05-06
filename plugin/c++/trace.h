#ifndef TRACE_H
#define TRACE_H

#include <sys/time.h>

struct tracer
{
  static unsigned int level;

  tracer (char const *func, bool timing);
  ~tracer ();

  char const *func;
  bool timing;
  timeval start;
};

#define TRACE tracer trace (__func__, false)
#define TIME tracer trace (__func__, true)

#endif /* TRACE_H */
