#if 0
%
% (c) The GRASP/AQUA Project, Glasgow University, 1996
%
\subsection[listenSocket.lc]{Indicate willingness to receive connections}

\begin{code}
#endif

#define NON_POSIX_SOURCE
#include "rtsdefs.h"
#include "ghcSockets.h"

StgInt
listenSocket(I_ sockfd, I_ backlog)
{
    int rc;
    
    while ((rc = listen((int) sockfd, (int) backlog)) < 0) {
      if (errno != EINTR) {
	  cvtErrno();
	  stdErrno();
	  return -1;
      }
    }
    return 0;
}
