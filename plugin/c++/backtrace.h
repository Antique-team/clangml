#ifndef BACKTRACE_H
#define BACKTRACE_H

#include <sys/param.h> // OS detection
#ifndef __FreeBSD__
#ifndef __APPLE__

void backtrace_init ();

#endif  /* __APPLE__ */
#endif  /* __FreeBSD__ */

#endif
