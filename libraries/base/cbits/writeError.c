/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1998
 *
 * $Id: writeError.c,v 1.1 2001/06/28 14:15:04 simonmar Exp $
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
#include "HsCore.h"

HsAddr
addrOf_ErrorHdrHook(void)
{
  return &ErrorHdrHook;
}

void
writeErrString__ (HsAddr msg_hdr, HsAddr msg, HsInt len)
{
  int count = 0;
  char* p  = (char*)msg;
  char  nl = '\n';

  resetNonBlockingFd(2);

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
