/* -----------------------------------------------------------------------------
 * $Id: recvFrom.c,v 1.3 1998/12/02 13:26:46 simonm Exp $
 *
 * recvFrom run-time support
 *
 * (c) The GHC Team 1998
 * -------------------------------------------------------------------------- */

#define NON_POSIX_SOURCE
#include "Rts.h"
#include "ghcSockets.h"
#include "stgio.h"

StgInt
recvFrom__(StgInt fd, StgAddr buf, StgInt nbytes, StgAddr from)
{
  StgInt count;
  int sz;
  int flags = 0;

  sz = sizeof(struct sockaddr_in);

  while ( (count = recvfrom((int)fd, (void*)buf, (int)nbytes, flags, (struct sockaddr*)from, &sz)) < 0) {
      if (errno != EINTR) {
	  cvtErrno();
	  stdErrno();
	  return -1;
      }
  }
  return count;
}
