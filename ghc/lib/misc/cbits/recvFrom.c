#if 0
%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\subsection[recvFrom.lc]{recvFrom run-time support}

\begin{code}
#endif

#define NON_POSIX_SOURCE
#include "rtsdefs.h"
#include "ghcSockets.h"

StgInt
recvFrom__(fd, buf, nbytes, from)
StgInt fd;
StgAddr buf;
StgInt nbytes;
StgAddr from;
{
  StgInt count;
  int sz;
  int flags = 0;

  sz = sizeof(struct sockaddr_in);

  while ( (count = recvfrom((int)fd, (void*)buf, (int)nbytes, flags, (struct sockaddr*)from, &sz)) < 0) {
      if (errno != EINTR) {
	  cvtErrno();
	  stdErrno();
	  return -1;
      }
  }
  return count;
}
