/* ----------------------------------------------------------------------------
   (c) The University of Glasgow 2004

   Interface for code in cbits/execvpe.c
   ------------------------------------------------------------------------- */

#ifndef HSUNIX_EXECVPE_H
#define HSUNIX_EXECVPE_H

extern int
__hsunix_execvpe(const char *name, char *const argv[], char *const envp[]);

// this hack is needed for `process`; to be removed in unix-2.8
#ifndef HSUNIX_EXECVPE_H_NO_COMPAT
#include "HsUnixConfig.h"
#if HAVE_EXECVPE
# include <unistd.h>
extern int
execvpe(const char *name, char *const argv[], char *const envp[]);
#else
# define execvpe(name,argv,envp) __hsunix_execvpe(name,argv,envp)
#endif
#endif

#endif
