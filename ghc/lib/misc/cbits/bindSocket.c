#if 0
%
% (c) The GRASP/AQUA Project, Glasgow University, 1995
%
\subsection[bindSocket.lc]{Assign name to unnamed socket}

\begin{code}
#endif

#define NON_POSIX_SOURCE
#include "rtsdefs.h"
#include "ghcSockets.h"

StgInt
bindSocket(I_ sockfd, A_ myaddr, I_ addrlen, I_ isUnixDomain)
{
    int rc;
    
    while ((rc = bind((int)sockfd, (struct sockaddr *)myaddr, (int)addrlen)) < 0) {
      if (errno != EINTR) {
	  cvtErrno();
	  stdErrno();
	  return -1;
      }
    }
    return 0;
}
