/* 
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1998
 *
 * $Id: getCurrentDirectory.c,v 1.3 1998/12/02 13:27:39 simonm Exp $
 *
 * getCurrentDirectory Runtime Support
 */

#include "Rts.h"
#include "stgio.h"

#ifndef PATH_MAX
#ifdef  MAXPATHLEN
#define PATH_MAX MAXPATHLEN
#else
#define PATH_MAX 1024
#endif
#endif

StgAddr
getCurrentDirectory(void)
{
    char *pwd;
    int alloc;

    alloc = PATH_MAX;
    if ((pwd = malloc(alloc)) == NULL) {
	ghc_errtype = ERR_RESOURCEEXHAUSTED;
	ghc_errstr = "not enough virtual memory";
	return NULL;
    }
    while (getcwd(pwd, alloc) == NULL) {
	if (errno == ERANGE) {
	    alloc += PATH_MAX;
	    if ((pwd = realloc(pwd, alloc)) == NULL) {
		ghc_errtype = ERR_RESOURCEEXHAUSTED;
		ghc_errstr = "not enough virtual memory";
		return NULL;
	    }
	} else if (errno != EINTR) {
	    cvtErrno();
	    stdErrno();
	    return NULL;
	}
    }
    return (StgAddr) pwd;
}
