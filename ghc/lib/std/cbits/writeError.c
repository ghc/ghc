/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1998
 *
 * $Id: writeError.c,v 1.3 1999/11/05 15:22:59 simonmar Exp $
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
#include "stgio.h"

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

void
writeErrString__ (msg_hdr, msg, len)
StgAddr msg_hdr;
StgByteArray msg;
StgInt len;
{
  int count = 0;
  char* p  = (char*)msg;
  char  nl = '\n';
  long fd_flags;

#if !defined(_WIN32) || defined(__CYGWIN__) || defined(__CYGWIN32__)
    /* clear the non-blocking flag on this file descriptor */
    fd_flags = fcntl(2, F_GETFL);
    fcntl(2, F_SETFL, fd_flags & ~O_NONBLOCK);
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
