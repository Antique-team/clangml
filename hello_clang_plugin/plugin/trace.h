#ifndef TRACE_H
#define TRACE_H

#include <cstdio>


static unsigned int level = 0;
struct tracer
{
  tracer (char const *func)
    : func (func)
  {
    std::printf ("%*s> %s\n", level++, "", func);
  }

  ~tracer ()
  {
    std::printf ("%*s< %s\n", --level, "", func);
  }

  char const *func;
};

#define TRACE tracer trace (__func__)
#define TODO std::printf ("TODO: %s\n", __func__)

#endif /* TRACE_H */
