#if 0
%
% (c) The GRASP/AQUA Project, Glasgow University, 1996
%
\subsection[writeDescriptor.lc]{Stuff bytes down a descriptor}

\begin{code}
#endif

#define NON_POSIX_SOURCE
#include "rtsdefs.h"
#include "ghcSockets.h"

StgInt
writeDescriptor(I_ fd, A_ buf, I_ nbytes)
{
    StgInt dumped;
    
    while ((dumped = write((int) fd, (char *) buf, (int) nbytes)) < 0) {
      if (errno != EINTR) {
	  cvtErrno();
	  stdErrno();
	  return -1;
      }
    }
    return dumped;
}
