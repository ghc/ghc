#if 0
%
% (c) The GRASP/AQUA Project, Glasgow University, 1996
%
\subsection[getSockName.lc]{Return name of process assoc with socket}

\begin{code}
#endif

#define NON_POSIX_SOURCE
#include "rtsdefs.h"
#include "ghcSockets.h"

StgInt
getSockName(I_ sockfd, A_ peer, A_ namelen)
{
    StgInt name;
    
    while ((name = getsockname((int) sockfd, (struct sockaddr *) peer, (int *) namelen)) < 0) {
      if (errno != EINTR) {
	  cvtErrno();
	  stdErrno();
	  return -1;
      }
    }
    return name;
}
