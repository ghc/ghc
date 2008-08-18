/* 
 * (c) The University of Glasgow 2002
 *
 * Directory Runtime Support
 */

/* needed only for solaris2_HOST_OS */
#include "ghcconfig.h"

// The following is required on Solaris to force the POSIX versions of
// the various _r functions instead of the Solaris versions.
#ifdef solaris2_HOST_OS
#define _POSIX_PTHREAD_SEMANTICS
#endif

#include "HsBase.h"

#if defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32)
#include <windows.h>
#endif


/*
 * read an entry from the directory stream; opt for the
 * re-entrant friendly way of doing this, if available.
 */
int
__hscore_readdir( DIR *dirPtr, struct dirent **pDirEnt )
{
#if HAVE_READDIR_R
  struct dirent* p;
  int res;
  static unsigned int nm_max = (unsigned int)-1;
  
  if (pDirEnt == NULL) {
    return -1;
  }
  if (nm_max == (unsigned int)-1) {
#ifdef NAME_MAX
    nm_max = NAME_MAX + 1;
#else
    nm_max = pathconf(".", _PC_NAME_MAX);
    if (nm_max == -1) { nm_max = 255; }
    nm_max++;
#endif
  }
  p = (struct dirent*)malloc(sizeof(struct dirent) + nm_max);
  if (p == NULL) return -1;
  res = readdir_r(dirPtr, p, pDirEnt);
  if (res != 0) {
      *pDirEnt = NULL;
      free(p);
  }
  else if (*pDirEnt == NULL) {
    // end of stream
    free(p);
  }
  return res;
#else

  if (pDirEnt == NULL) {
    return -1;
  }

  *pDirEnt = readdir(dirPtr);
  if (*pDirEnt == NULL) {
    return -1;
  } else {
    return 0;
  }  
#endif
}
