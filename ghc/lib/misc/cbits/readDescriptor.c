#if 0
%
% (c) The GRASP/AQUA Project, Glasgow University, 1996
%
\subsection[readDescriptor.lc]{Suck some bytes from a descriptor}

\begin{code}
#endif

#define NON_POSIX_SOURCE
#include "Rts.h"
#include "ghcSockets.h"
#include "stgio.h"

StgInt
readDescriptor(I_ fd, A_ buf, I_ nbytes)
{
    StgInt sucked;
    
    while ((sucked = read((int) fd, (char *) buf, (int) nbytes)) < 0) {
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
	  case GHC_EBADMSG:
       	      ghc_errtype = ERR_SYSTEMERROR;
              ghc_errstr  = "Message waiting to be read is not a data message";
	      break;
	  case GHC_EFAULT:
       	      ghc_errtype = ERR_INVALIDARGUMENT;
              ghc_errstr  = "Data buffer not in writeable part of user address space";
	      break;
	  case GHC_EINVAL:
	      ghc_errtype = ERR_INVALIDARGUMENT;
	      ghc_errstr  = "Seek pointer associated with descriptor negative";
	      break;
	  case GHC_EIO:
	      ghc_errtype = ERR_SYSTEMERROR;
	      ghc_errstr  = "I/O error occurred while writing to file system";
	      break;
	  case GHC_EISDIR:
	      ghc_errtype = ERR_INAPPROPRIATETYPE;
	      ghc_errstr  = "Descriptor refers to a directory";
	      break;
	  case GHC_EAGAIN:
	  case GHC_EWOULDBLOCK:
	      ghc_errtype = ERR_OTHERERROR;
	      ghc_errstr  = "No data could be written immediately";
	      break;
	  }
	  return -1;
      }
    }
    return sucked;
}
