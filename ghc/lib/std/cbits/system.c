/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: system.c,v 1.6 1999/12/08 15:47:08 simonmar Exp $
 *
 * system Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifndef mingw32_TARGET_OS
# ifdef HAVE_SYS_WAIT_H
#  include <sys/wait.h>
# endif
#endif

#ifdef HAVE_VFORK_H
#include <vfork.h>
#endif

#ifdef HAVE_VFORK
#define fork vfork
#endif

StgInt
systemCmd(StgByteArray cmd)
{
#if defined(mingw32_TARGET_OS)
  if (system(cmd) < 0) {
     cvtErrno();
     stdErrno();
     return -1;
  }
  sleep(1);
  return 0;
#else
#if defined(cygwin32_TARGET_OS)
   /* The implementation of std. fork() has its problems
      under cygwin32-b18, so we fall back on using libc's
      system() instead. (It in turn has problems, as it
      does not wait until the sub shell has finished before
      returning. Using sleep() works around that.)
  */
  if (system(cmd) < 0) {
     cvtErrno();
     stdErrno();
     return -1;
  }
  sleep(1);
  return 0;
#else
    int pid;
    int wstat;

    switch(pid = fork()) {
    case -1:
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
    case 0:
	/* the child */
	execl("/bin/sh", "sh", "-c", cmd, NULL);
	_exit(127);
    }

    while (waitpid(pid, &wstat, 0) < 0) {
	if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return -1;
	}
    }

    if (WIFEXITED(wstat))
	return WEXITSTATUS(wstat);
    else if (WIFSIGNALED(wstat)) {
	ghc_errtype = ERR_INTERRUPTED;
	ghc_errstr = "system command interrupted";
    }
    else {
	/* This should never happen */
	ghc_errtype = ERR_OTHERERROR;
	ghc_errstr = "internal error (process neither exited nor signalled)";
    }
    return -1;
#endif
#endif
}
