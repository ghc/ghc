/* 
 * (c) The University of Glasgow 2002
 *
 * $Id: system.c,v 1.8 2003/07/02 13:27:35 stolz Exp $
 *
 * system Runtime Support
 */

/* The itimer stuff in this module is non-posix */
// #include "PosixSource.h"

#include "HsBase.h"

#if defined(mingw32_TARGET_OS)
#include <windows.h>
#include <stdlib.h>
#endif

#ifdef HAVE_VFORK_H
#include <vfork.h>
#endif

#ifdef HAVE_VFORK
#define fork vfork
#endif

HsInt
systemCmd(HsAddr cmd)
{
  /* -------------------- WINDOWS VERSION --------------------- */
#if defined(mingw32_TARGET_OS) || defined(cygwin32_TARGET_OS)
    return system(cmd);
#else
  /* -------------------- UNIX VERSION --------------------- */
    int pid;
    int wstat;

    switch(pid = fork()) {
    case -1:
	{
	    return -1;
	}
    case 0:
      {
#ifdef HAVE_SETITIMER
	/* Reset the itimers in the child, so it doesn't get plagued
	 * by SIGVTALRM interrupts.
	 */
	struct timeval tv_null = { 0, 0 };
	struct itimerval itv;
	itv.it_interval = tv_null;
	itv.it_value = tv_null;
	setitimer(ITIMER_REAL, &itv, NULL);
	setitimer(ITIMER_VIRTUAL, &itv, NULL);
	setitimer(ITIMER_PROF, &itv, NULL);
#endif

	/* the child */
	execl("/bin/sh", "sh", "-c", cmd, NULL);
	_exit(127);
      }
    }

    while (waitpid(pid, &wstat, 0) < 0) {
	if (errno != EINTR) {
	    return -1;
	}
    }

    if (WIFEXITED(wstat))
	return WEXITSTATUS(wstat);
    else if (WIFSIGNALED(wstat)) {
	errno = EINTR;
    }
    else {
	/* This should never happen */
    }
    return -1;
#endif
}
