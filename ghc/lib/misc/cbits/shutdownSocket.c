#if 0
%
% (c) The GRASP/AQUA Project, Glasgow University, 1996
%
\subsection[shutdownSocket.lc]{Shut down part of full duplex connection}

\begin{code}
#endif

#define NON_POSIX_SOURCE
#include "rtsdefs.h"
#include "ghcSockets.h"

StgInt
shutdownSocket(I_ sockfd, I_ how)
{
    StgInt rc;
    
    while ((rc = shutdown((int) sockfd, (int) how)) < 0) {
      if (errno != EINTR) {
	  cvtErrno();
	  switch (ghc_errno) {
	  default:
	      stdErrno();
	      break;
	  case GHC_EBADF:
       	      ghc_errtype = ERR_INVALIDARGUMENT;
              ghc_errstr  = "Not a valid write descriptor";
	      break;
	  case GHC_ENOTCONN:
	      ghc_errtype = ERR_INVALIDARGUMENT;
	      ghc_errstr  = "Socket not connected";
	      break;
	  case GHC_ENOTSOCK:
	      ghc_errtype = ERR_INVALIDARGUMENT;
	      ghc_errstr  = "Descriptor is not a socket";
	      break;
	  }
	  return -1;
      }
    }
    return rc;
}
