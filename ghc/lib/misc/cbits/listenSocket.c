#if 0
%
% (c) The GRASP/AQUA Project, Glasgow University, 1996
%
\subsection[listenSocket.lc]{Indicate willingness to receive connections}

\begin{code}
#endif

#define NON_POSIX_SOURCE
#include "Rts.h"
#include "ghcSockets.h"
#include "stgio.h"

StgInt
listenSocket(I_ sockfd, I_ backlog)
{
    int rc;
    
    while ((rc = listen((int) sockfd, (int) backlog)) < 0) {
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
	  case GHC_ENOTSOCK:
	      ghc_errtype = ERR_INVALIDARGUMENT;
	      ghc_errstr  = "Descriptor not a socket";
	      break;
	  case GHC_EOPNOTSUPP:
	      ghc_errtype = ERR_INVALIDARGUMENT;
	      ghc_errstr  = "Socket not of type that supports listen";
	      break;
	  }
	  return -1;
      }
    }
    return 0;
}
