#if 0
%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\subsection[sendTo.c]{sendTo run-time support}

\begin{code}
#endif

#define NON_POSIX_SOURCE
#include "rtsdefs.h"
#include "ghcSockets.h"

StgInt
sendTo__(fd, buf, nbytes, to, sz)
StgInt fd;
StgAddr buf;
StgInt nbytes;
StgAddr to;
StgInt  sz;
{
  StgInt count;
  int flags = 0;

  while ( (count = sendto((int)fd, (void*)buf, (int)nbytes, flags, (struct sockaddr*)to, sz)) < 0) {
      if (errno != EINTR) {
	  cvtErrno();
	  stdErrno();
	  return -1;
      }
  }
  return count;
}
