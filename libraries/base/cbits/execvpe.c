/* -----------------------------------------------------------------------------
   (c) The University of Glasgow 1995-2004

   Our low-level exec() variant.
   -------------------------------------------------------------------------- */
#include "HsBase.h"

#if !(defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32)) /* to the end */

/* Evidently non-Posix. */
/* #include "PosixSource.h" */

#include <unistd.h>
#include <sys/time.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

/*
 * We want the search semantics of execvp, but we want to provide our
 * own environment, like execve.  The following copyright applies to
 * this code, as it is a derivative of execvp:
 *-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

int
execvpe(char *name, char *const argv[], char **envp)
{
    register int lp, ln;
    register char *p;
    int eacces=0, etxtbsy=0;
    char *bp, *cur, *path, *buf = 0;

    /* If it's an absolute or relative path name, it's easy. */
    if (strchr(name, '/')) {
	bp = (char *) name;
	cur = path = buf = NULL;
	goto retry;
    }

    /* Get the path we're searching. */
    if (!(path = getenv("PATH"))) {
#ifdef HAVE_CONFSTR
        ln = confstr(_CS_PATH, NULL, 0);
        if ((cur = path = malloc(ln + 1)) != NULL) {
	    path[0] = ':';
	    (void) confstr (_CS_PATH, path + 1, ln);
	}
#else
        if ((cur = path = malloc(1 + 1)) != NULL) {
	    path[0] = ':';
	    path[1] = '\0';
	}
#endif
    } else
	cur = path = strdup(path);

    if (path == NULL || (bp = buf = malloc(strlen(path)+strlen(name)+2)) == NULL)
	goto done;

    while (cur != NULL) {
	p = cur;
        if ((cur = strchr(cur, ':')) != NULL)
	    *cur++ = '\0';

	/*
	 * It's a SHELL path -- double, leading and trailing colons mean the current
	 * directory.
	 */
	if (!*p) {
	    p = ".";
	    lp = 1;
	} else
	    lp = strlen(p);
	ln = strlen(name);

	memcpy(buf, p, lp);
	buf[lp] = '/';
	memcpy(buf + lp + 1, name, ln);
	buf[lp + ln + 1] = '\0';

      retry:
        (void) execve(bp, argv, envp);
	switch (errno) {
	case EACCES:
	    eacces = 1;
	    break;
	case ENOENT:
	    break;
	case ENOEXEC:
	    {
		register size_t cnt;
		register char **ap;

		for (cnt = 0, ap = (char **) argv; *ap; ++ap, ++cnt)
		    ;
		if ((ap = malloc((cnt + 2) * sizeof(char *))) != NULL) {
		    memcpy(ap + 2, argv + 1, cnt * sizeof(char *));

		    ap[0] = "sh";
		    ap[1] = bp;
		    (void) execve("/bin/sh", ap, envp);
		    free(ap);
		}
		goto done;
	    }
	case ETXTBSY:
	    if (etxtbsy < 3)
		(void) sleep(++etxtbsy);
	    goto retry;
	default:
	    goto done;
	}
    }
    if (eacces)
	errno = EACCES;
    else if (!errno)
	errno = ENOENT;
  done:
    if (path)
	free(path);
    if (buf)
	free(buf);
    return (-1);
}


/* Copied verbatim from ghc/lib/std/cbits/system.c. */
void pPrPr_disableITimers (void)
{
#  ifdef HAVE_SETITIMER
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
#  endif
}

#endif
