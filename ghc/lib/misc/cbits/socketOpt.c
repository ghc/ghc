#if 0
%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\subsection[socketOpt.lc]{Setting/Getting socket opts}

\begin{code}
#endif

#define NON_POSIX_SOURCE
#include "Rts.h"
#include "ghcSockets.h"
#include "stgio.h"

StgInt
getSocketOption__ (StgInt fd, StgInt opt, StgInt level)
{
  int optval, sz_optval, rc;

  sz_optval = sizeof(int);

  while ( (rc = getsockopt((int)fd, level, opt, &optval, &sz_optval)) < 0 ) {
      if (errno != EINTR) {
	  cvtErrno();
	  stdErrno();
	  return -1;
      }
   }
   return optval;
}

StgInt
setSocketOption__ (StgInt fd, StgInt opt, StgInt level, StgInt val)
{
  int optval, rc;

  optval = val;

  while ( (rc = setsockopt((int)fd, level, opt, &optval, sizeof(optval))) < 0 ) {
      if (errno != EINTR) {
	  cvtErrno();
	  stdErrno();
	  return -1;
      }
   }
   return 0;
}
