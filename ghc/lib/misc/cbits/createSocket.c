#if 0
%
% (c) The GRASP/AQUA Project, Glasgow University, 1995
%
\subsection[createSocket.lc]{Create a socket file descriptor}

\begin{code}
#endif

#define NON_POSIX_SOURCE
#include "rtsdefs.h"
#include "ghcSockets.h"

StgInt
createSocket(I_ family, I_ type, I_ protocol)
{
    int fd;

    if ((fd = socket((int)family, (int)type, (int)protocol)) < 0) {
      if (errno != EINTR) {
	  cvtErrno();
	  stdErrno();
	  return -1;
      }
    }
    return fd;
}
