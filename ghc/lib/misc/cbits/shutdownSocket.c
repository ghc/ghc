#if 0
%
% (c) The GRASP/AQUA Project, Glasgow University, 1996
%
\subsection[shutdownSocket.lc]{Shut down part of full duplex connection}

\begin{code}
#endif

#define NON_POSIX_SOURCE
#include "rtsdefs.h"
#include "ghcSockets.h"

StgInt
shutdownSocket(I_ sockfd, I_ how)
{
    StgInt rc;
    
    while ((rc = shutdown((int) sockfd, (int) how)) < 0) {
      if (errno != EINTR) {
	  cvtErrno();
	  stdErrno();
	  return -1;
      }
    }
    return rc;
}
