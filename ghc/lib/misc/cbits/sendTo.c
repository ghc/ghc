/* -----------------------------------------------------------------------------
 * $Id: sendTo.c,v 1.3 1998/12/02 13:26:46 simonm Exp $
 *
 * sendTo run-time support
 *
 * (c) The GHC Team 1998
 * -------------------------------------------------------------------------- */

#define NON_POSIX_SOURCE
#include "Rts.h"
#include "ghcSockets.h"
#include "stgio.h"

StgInt
sendTo__(StgInt fd, StgAddr buf, StgInt nbytes, StgAddr to, StgInt sz)
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
