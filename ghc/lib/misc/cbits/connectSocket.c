#if 0
%
% (c) The GRASP/AQUA Project, Glasgow University, 1995
%
\subsection[connectSocket.lc]{Assign name to client socket}

\begin{code}
#endif

#define NON_POSIX_SOURCE
#include "rtsdefs.h"
#include "ghcSockets.h"

StgInt
connectSocket(sockfd, servaddr, addrlen, isUnixDomain)
StgInt  sockfd;
StgAddr servaddr;
StgInt  addrlen;
StgInt isUnixDomain;
{
    int rc;
    
    while ((rc = connect((int)sockfd, (struct sockaddr *)servaddr, (int)addrlen)) < 0) {
      if (errno != EINTR) {
	  cvtErrno();
	  stdErrno();
	  return -1;
      }
    }
    return 0;
}
