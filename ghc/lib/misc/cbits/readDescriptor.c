#if 0
%
% (c) The GRASP/AQUA Project, Glasgow University, 1996
%
\subsection[readDescriptor.lc]{Suck some bytes from a descriptor}

\begin{code}
#endif

#define NON_POSIX_SOURCE
#include "rtsdefs.h"
#include "ghcSockets.h"

StgInt
readDescriptor(I_ fd, A_ buf, I_ nbytes)
{
    StgInt sucked;
    
    while ((sucked = read((int) fd, (char *) buf, (int) nbytes)) < 0) {
      if (errno != EINTR) {
	  cvtErrno();
	  stdErrno();
	  return -1;
      }
    }
    return sucked;
}
