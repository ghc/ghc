#if 0
%
% (c) The GRASP/AQUA Project, Glasgow University, 1996
%
\subsection[acceptSocket.lc]{Server wait for client to connect}

\begin{code}
#endif

#define NON_POSIX_SOURCE
#include "rtsdefs.h"
#include "ghcSockets.h"

StgInt
acceptSocket(I_ sockfd, A_ peer, A_ addrlen)
{
    StgInt fd;
    
    while ((fd = accept((int)sockfd, (struct sockaddr *)peer, (int *)addrlen)) < 0) {
      if (errno != EINTR) {
	  cvtErrno();
	  stdErrno();
	  return -1;
      }
    }
    return fd;
}
