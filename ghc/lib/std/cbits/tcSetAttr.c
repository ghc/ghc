/* 
 * (c) The GHC Team 2001
 *
 * $Id: tcSetAttr.c,v 1.2 2001/01/26 17:51:40 rrt Exp $
 *
 * A wrapper around tcsetattr() which works for a background process.
 */

#include "Rts.h"
#include "stgio.h"

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

#ifdef HAVE_TERMIOS_H
#include <termios.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifndef mingw32_TARGET_OS
/* tcsetattr() when invoked by a background process causes the process
 * to be sent SIGTTOU regardless of whether the process has TOSTOP set
 * in its terminal flags (try it...).  This function provides a
 * wrapper which temporarily blocks SIGTTOU around the call, making it
 * transparent.  */
int
tcSetAttr( int fd, int options, const struct termios *tp )
{
    int res;
    sigset_t block_ttou, old_sigset;
    
    sigemptyset (&block_ttou);
    sigaddset (&block_ttou, SIGTTOU);
    sigprocmask(SIG_BLOCK, &block_ttou, &old_sigset);
    res = tcsetattr(fd, options, tp);
    sigprocmask(SIG_SETMASK, &old_sigset, NULL);

    return res;
}
#else
#define tcSetAttr(f,o,t) tcsetattr((f),(o),(t))
#endif
