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
	  switch (ghc_errno) {
	  case GHC_EBADF:
       	      ghc_errtype = ERR_INVALIDARGUMENT;
              ghc_errstr  = "Not a valid read descriptor";
	      break;
	  case GHC_EBADMSG:
       	      ghc_errtype = ERR_SYSTEMERROR;
              ghc_errstr  = "Message waiting to be read is not a data message";
	      break;
	  case GHC_EFAULT:
       	      ghc_errtype = ERR_INVALIDARGUMENT;
              ghc_errstr  = "Data buffer not in readable part of user address space";
	      break;
	  case GHC_EINVAL:
	      ghc_errtype = ERR_INVALIDARGUMENT;
	      ghc_errstr  = "Seek pointer associated with descriptor negative";
	      break;
	  case GHC_EIO:
	      ghc_errtype = ERR_SYSTEMERROR;
	      ghc_errstr  = "I/O error occurred while reading";
	      break;
	  case GHC_EISDIR:
	      ghc_errtype = ERR_INAPPROPRIATETYPE;
	      ghc_errstr  = "Descriptor refers to a directory";
	      break;
	  case GHC_EAGAIN:
	  case GHC_EWOULDBLOCK:
	      ghc_errtype = ERR_OTHERERROR;
	      ghc_errstr  = "No data could be read immediately";
	      break;
	  default:
	      stdErrno();
	      break;
	  }
	  return -1;
      }
    }
    return count;
}
