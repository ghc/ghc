/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * TTY-related functionality
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include "RtsUtils.h" // __hscore_get/set prototypes
#include "TTY.h"

#ifdef HAVE_TERMIOS_H
#include <termios.h>
#endif
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

// Here we save the terminal settings on the standard file
// descriptors, if we need to change them (eg. to support NoBuffering
// input).
static void *saved_termios[3] = {NULL,NULL,NULL};

void*
__hscore_get_saved_termios(int fd)
{
  return (0 <= fd &&
            fd < (int)(sizeof(saved_termios) / sizeof(*saved_termios))) ?
  saved_termios[fd] : NULL;
}

void
__hscore_set_saved_termios(int fd, void* ts)
{
  if (0 <= fd && fd < (int)(sizeof(saved_termios) / sizeof(*saved_termios))) {
    saved_termios[fd] = ts;
  }
}

void
resetTerminalSettings (void)
{
#if HAVE_TERMIOS_H
    // Reset the terminal settings on the standard file descriptors,
    // if we changed them.  See System.Posix.Internals.tcSetAttr for
    // more details, including the reason we termporarily disable
    // SIGTTOU here.
    {
        int fd;
        sigset_t sigset, old_sigset;
        sigemptyset(&sigset);
        sigaddset(&sigset, SIGTTOU);
        sigprocmask(SIG_BLOCK, &sigset, &old_sigset);
        for (fd = 0; fd <= 2; fd++) {
            struct termios* ts =
                (struct termios*)__hscore_get_saved_termios(fd);
            if (ts != NULL) {
                tcsetattr(fd,TCSANOW,ts);
            }
        }
        sigprocmask(SIG_SETMASK, &old_sigset, NULL);
    }
#endif
}
