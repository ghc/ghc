#if 0
%
% (c) The GRASP/AQUA Project, Glasgow University, 1996
%
\subsection[getPeerName.lc]{Return name of peer process}

Returns name of peer process connected to a socket.

\begin{code}
#endif

#define NON_POSIX_SOURCE
#include "rtsdefs.h"
#include "ghcSockets.h"

StgInt
getPeerName(I_ sockfd, A_ peer, A_ namelen)
{
    StgInt name;
    
    while ((name = getpeername((int) sockfd, (struct sockaddr *) peer, (int *) namelen)) < 0) {
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
	  case GHC_EFAULT:
       	      ghc_errtype = ERR_INVALIDARGUMENT;
              ghc_errstr  = "Data not in writeable part of user address space";
	      break;
	  case GHC_ENOBUFS:
	      ghc_errtype = ERR_RESOURCEEXHAUSTED;
	      ghc_errstr  = "Insuffcient resources";
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
    return name;
}
