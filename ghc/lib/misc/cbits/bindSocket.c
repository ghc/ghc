#if 0
%
% (c) The GRASP/AQUA Project, Glasgow University, 1995
%
\subsection[bindSocket.lc]{Assign name to unnamed socket}

\begin{code}
#endif

#define NON_POSIX_SOURCE
#include "rtsdefs.h"
#include "ghcSockets.h"

StgInt
bindSocket(I_ sockfd, A_ myaddr, I_ addrlen, I_ isUnixDomain)
{
    int rc;
    
    while ((rc = bind((int)sockfd, (struct sockaddr *)myaddr, (int)addrlen)) < 0) {
      if (errno != EINTR) {
	  cvtErrno();
	  switch (ghc_errno) {
	  default:
	      stdErrno();
	      break;
	  case GHC_EACCES:
       	      ghc_errtype = ERR_PERMISSIONDENIED;
	      if (isUnixDomain != 0)
	         ghc_errstr = "For a component of path prefix of path name";
	      else
	         ghc_errstr  = "Requested address protected, cannot bind socket";
	      break;
	  case GHC_EISCONN:
	  case GHC_EADDRINUSE:
	      ghc_errtype = ERR_RESOURCEBUSY;
	      ghc_errstr  = "Address already in use";
	      break;
	  case GHC_EADDRNOTAVAIL:
	      ghc_errtype = ERR_PERMISSIONDENIED;
	      ghc_errstr  = "Address not available from local machine";
	      break;
	  case GHC_EBADF:
	      ghc_errtype = ERR_INVALIDARGUMENT;
	      ghc_errstr  = "Not a valid socket file descriptor";
	      break;
	  case GHC_EFAULT:
	      ghc_errtype = ERR_INVALIDARGUMENT;
	      ghc_errstr  = "Address not in valid part of user address space";
	      break;
	  case GHC_EINVAL:
	      ghc_errtype = ERR_SYSTEMERROR;
	      ghc_errstr  = "Specified size of structure not equal valid address for family";
	      break;
	  case GHC_ENOTSOCK:
	      ghc_errtype = ERR_INAPPROPRIATETYPE;
	      ghc_errstr  = "Descriptor for file, not a socket";
	      break;
	  case GHC_EIO:
	      ghc_errtype = ERR_SYSTEMERROR;
	      ghc_errstr  = "Could not make directory entry or alloc inode";
	      break;
	  case GHC_EISDIR:
	      ghc_errtype = ERR_INVALIDARGUMENT;
	      ghc_errstr  = "A null path name was given";
	      break;
	  case GHC_ELOOP:
	      ghc_errtype = ERR_SYSTEMERROR;
	      ghc_errstr  = "Too many symbolic links encountered";
	      break;
	  case GHC_ENAMETOOLONG:
	      ghc_errtype = ERR_INVALIDARGUMENT;
	      ghc_errstr  = "Max length of path name exceeded";
	      break;
	  case GHC_ENOENT:
	      ghc_errtype = ERR_INVALIDARGUMENT;
	      ghc_errstr  = "Component in path prefix does not exist";
	      break;
	  case GHC_ENOTDIR:
	      ghc_errtype = ERR_INVALIDARGUMENT;
	      ghc_errstr  = "Component in path prefix is not a directory";
	      break;
	  case GHC_EROFS:
	      ghc_errtype = ERR_INVALIDARGUMENT;
	      ghc_errstr  = "The inode would reside on read only file system";
	      break;
	  }
	  return -1;
      }
    }
    return 0;
}
