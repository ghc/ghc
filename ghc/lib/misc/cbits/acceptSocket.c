#if 0
%
% (c) The GRASP/AQUA Project, Glasgow University, 1996
%
\subsection[acceptSocket.lc]{Server wait for client to connect}

\begin{code}
#endif

#define NON_POSIX_SOURCE
#include "Rts.h"
#include "ghcSockets.h"
#include "stgio.h"

StgInt
acceptSocket(I_ sockfd, A_ peer, A_ addrlen)
{
    StgInt fd;
    
    while ((fd = accept((int)sockfd, (struct sockaddr *)peer, (int *)addrlen)) < 0) {
      if (errno != EINTR) {
	  cvtErrno();
	  switch (ghc_errno) {
	  default:
	      stdErrno();
	      break;
	  case GHC_EBADF:
       	      ghc_errtype = ERR_INVALIDARGUMENT;
              ghc_errstr  = "Not a valid descriptor";
	      break;
	  case GHC_EFAULT:
       	      ghc_errtype = ERR_INVALIDARGUMENT;
              ghc_errstr  = "Address not in writeable part of user address space";
	      break;
	  case GHC_ENOTSOCK:
	      ghc_errtype = ERR_INVALIDARGUMENT;
	      ghc_errstr  = "Descriptor not a socket";
	      break;
	  case GHC_EOPNOTSUPP:
	      ghc_errtype = ERR_INVALIDARGUMENT;
	      ghc_errstr  = "Socket not of type that supports listen";
	      break;
	  case GHC_EWOULDBLOCK:
	      ghc_errtype = ERR_OTHERERROR;
	      ghc_errstr  = "No sockets are present to be accepted";
	      break;
	  }
	  return -1;
      }
    }
    return fd;
}
