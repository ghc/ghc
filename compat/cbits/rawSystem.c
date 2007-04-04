/* 
 * (c) The University of Glasgow 1994-2004
 *
 * WARNING: this file is here for backwards compatibility only.  It is
 * not included as part of the base package, but is #included into the
 * compiler and the runghc utility when building either of these with 
 * an old version of GHC.
 *
 * shell-less system Runtime Support (see System.Cmd.rawSystem).
 */

/* The itimer stuff in this module is non-posix */
/* #include "PosixSource.h" */

#include "../../../includes/ghcconfig.h"

#include <stdio.h>
#include <stdlib.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

# ifdef TIME_WITH_SYS_TIME
#  include <sys/time.h>
#  include <time.h>
# else
#  ifdef HAVE_SYS_TIME_H
#   include <sys/time.h>
#  else
#   include <time.h>
#  endif
# endif

#include "HsFFI.h"

#if defined(mingw32_HOST_OS)
#include <windows.h>
#endif

#ifdef HAVE_VFORK_H
#include <vfork.h>
#endif

#ifdef HAVE_VFORK
#define fork vfork
#endif

#if defined(mingw32_HOST_OS)
/* -------------------- WINDOWS VERSION --------------------- */

HsInt
rawSystem(char *cmd)
{
  STARTUPINFO sInfo;
  PROCESS_INFORMATION pInfo;
  DWORD retCode;

  ZeroMemory(&sInfo, sizeof(sInfo));
  sInfo.cb = sizeof(sInfo);

  if (!CreateProcess(NULL, cmd, NULL, NULL, TRUE, 0, NULL, NULL, &sInfo, &pInfo)) {
    /* The 'TRUE' says that the created process should share
       handles with the current process.  This is vital to ensure
       that error messages sent to stderr actually appear on the screen.
       Since we are going to wait for the process to terminate anyway,
       there is no problem with such sharing. */

      errno = EINVAL; // ToDo: wrong, caller should use GetLastError()
      return -1;
  }
  WaitForSingleObject(pInfo.hProcess, INFINITE);
  if (GetExitCodeProcess(pInfo.hProcess, &retCode) == 0) {
      errno = EINVAL; // ToDo: wrong, caller should use GetLastError()
      return -1;
  }

  CloseHandle(pInfo.hProcess);
  CloseHandle(pInfo.hThread);
  return retCode;
}

#else
/* -------------------- UNIX VERSION --------------------- */

HsInt
rawSystem(char *cmd, char **args)
{
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
	execvp(cmd, args);
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
}
#endif
