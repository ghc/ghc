#if 0
%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\subsection[socketOpt.lc]{Setting/Getting socket opts}

\begin{code}
#endif

#define NON_POSIX_SOURCE
#include "rtsdefs.h"
#include "ghcSockets.h"

StgInt
getSocketOption__ (fd, opt)
StgInt fd;
StgInt opt;
{
  int level,optval, sz_optval,rc;

  if ( opt == TCP_MAXSEG || opt == TCP_NODELAY ) {
     level = IPPROTO_TCP;
  } else {
     level = SOL_SOCKET;
  }

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
setSocketOption__ (fd, opt, val)
StgInt fd;
StgInt opt;
StgInt val;
{
  int level, optval,rc;

  if ( opt == TCP_MAXSEG || opt == TCP_NODELAY ) {
     level = IPPROTO_TCP;
  } else {
     level = SOL_SOCKET;
  }
  
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
