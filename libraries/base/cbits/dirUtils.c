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

/*
 * Function: __hscore_renameFile()
 *
 * Provide Haskell98's semantics for renaming files and directories.
 * It mirrors that of POSIX.1's behaviour for rename() by overwriting
 * the target if it exists (the MS CRT implementation of rename() returns
 * an error
 *
 */
int
__hscore_renameFile( char *src, char *dest)
{
#if defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32)
    static int forNT = -1;
    
    /* ToDo: propagate error codes back */
    if (MoveFileA(src, dest)) {
	return 0;
    } else {
	;
    }
    
    /* Failed...it could be because the target already existed. */
    if ( !GetFileAttributes(dest) ) {
	/* No, it's not there - just fail. */
	maperrno();
	return (-1);
    }

    if (forNT == -1) {
	OSVERSIONINFO ovi;
	ovi.dwOSVersionInfoSize = sizeof(ovi);
	if ( !GetVersionEx(&ovi) ) {
	    maperrno();
	    return (-1);
	}
	forNT = ((ovi.dwPlatformId & VER_PLATFORM_WIN32_NT) != 0);
    }
    
    if (forNT) {
	/* Easy, go for MoveFileEx() */
	if ( MoveFileExA(src, dest, MOVEFILE_REPLACE_EXISTING) ) {
	    return 0;
	} else {
	    maperrno();
	    return (-1);
	}
    }

    /* No MoveFileEx() for Win9x, try deleting the target. */
    /* Similarly, if the MoveFile*() ops didn't work out under NT */
    if (DeleteFileA(dest)) {
	if (MoveFileA(src,dest)) {
	    return 0;
	} else {
	    maperrno();
	    return (-1);
	}
    } else {
	maperrno();
	return (-1);
    }
#else
    return rename(src,dest);
#endif
}

