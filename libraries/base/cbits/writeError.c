/* 
 * (c) The University of Glasgow 2002
 *
 * $Id: writeError.c,v 1.5 2002/02/07 11:13:30 simonmar Exp $
 *
 * hPutStr Runtime Support
 */

/*
Writing out error messages. This is done outside Haskell
(i.e., no use of the IO implementation is made), since it
might be in an unstable state (e.g., hClose stderr >> error "foo")

(A secondary reason is that ``error'' is used by the IO
implementation in one or two places.)

*/

#include "Rts.h"
#include "RtsUtils.h"
#include "HsBase.h"

void
writeErrString__(HsAddr msg_hdr, HsAddr msg, HsInt len)
{
  int count = 0;
  char* p  = (char*)msg;
  char  nl = '\n';

#ifndef DLLized
  resetNonBlockingFd(2);
#endif

  /* Print error msg header */
  if (msg_hdr) {
    ((void (*)(int))msg_hdr)(2/*stderr*/);
  }

  while ( (count = write(2,p,len)) < len) {
     if (errno != EINTR ) {
        return;
     }
     len -= count;
     p   += count;
  }
  write(2, &nl, 1);
}
