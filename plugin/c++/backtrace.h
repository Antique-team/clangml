#ifndef BACKTRACE_H
#define BACKTRACE_H

#include <sys/param.h> // OS detection
#ifndef __FreeBSD__

void backtrace_init ();

#endif  /* __FreeBSD__ */

#endif
