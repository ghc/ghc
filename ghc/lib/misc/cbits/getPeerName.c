#if 0
%
% (c) The GRASP/AQUA Project, Glasgow University, 1996
%
\subsection[getPeerName.lc]{Return name of peer process}

Returns name of peer process connected to a socket.

\begin{code}
#endif

#define NON_POSIX_SOURCE
#include "rtsdefs.h"
#include "ghcSockets.h"

StgInt
getPeerName(I_ sockfd, A_ peer, A_ namelen)
{
    StgInt name;
    
    while ((name = getpeername((int) sockfd, (struct sockaddr *) peer, (int *) namelen)) < 0) {
      if (errno != EINTR) {
	  cvtErrno();
	  stdErrno();
	  return -1;
      }
    }
    return name;
}
