/* 
 * (c) The University of Glasgow 2002
 *
 * Directory Runtime Support
 */

#include "ghcconfig.h"

// The following is required on Solaris to force the POSIX versions of
// the various _r functions instead of the Solaris versions.
#ifdef solaris2_HOST_OS
#define _POSIX_PTHREAD_SEMANTICS
#endif

#include "HsBase.h"

#if defined(mingw32_HOST_OS) || defined(__MINGW32__) || defined(_MSC_VER)
#include <windows.h>

static
int
toErrno(DWORD rc)
{
    switch (rc) {
    case ERROR_FILE_NOT_FOUND:    return ENOENT;
    case ERROR_PATH_NOT_FOUND:    return ENOENT;
    case ERROR_TOO_MANY_OPEN_FILES: return EMFILE;
    case ERROR_ACCESS_DENIED:     return EACCES;
    case ERROR_INVALID_HANDLE:    return EBADF; /* kinda sorta */
    case ERROR_NOT_ENOUGH_MEMORY: return ENOMEM;
    case ERROR_INVALID_ACCESS:    return EINVAL;
    case ERROR_INVALID_DATA:      return EINVAL;
    case ERROR_OUTOFMEMORY:       return ENOMEM;
    case ERROR_SHARING_VIOLATION: return EACCES;
    case ERROR_LOCK_VIOLATION:    return EACCES;
    case ERROR_ALREADY_EXISTS:    return EEXIST;
    case ERROR_BUSY:              return EBUSY;
    case ERROR_BROKEN_PIPE:       return EPIPE;
    case ERROR_PIPE_CONNECTED:    return EBUSY;
    case ERROR_PIPE_LISTENING:    return EBUSY;
    case ERROR_NOT_CONNECTED:     return EINVAL;

    case ERROR_NOT_OWNER:         return EPERM;
    case ERROR_DIRECTORY:         return ENOTDIR;
    case ERROR_FILE_INVALID:      return EACCES;
    case ERROR_FILE_EXISTS:       return EEXIST;

    default:
	return rc;
    }
}
#endif


/*
 * read an entry from the directory stream; opt for the
 * re-entrant friendly way of doing this, if available.
 */
HsInt
__hscore_readdir( HsAddr dirPtr, HsAddr pDirEnt )
{
  struct dirent **pDirE = (struct dirent**)pDirEnt;
#if HAVE_READDIR_R
  struct dirent* p;
  int res;
  static unsigned int nm_max = (unsigned int)-1;
  
  if (pDirE == NULL) {
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
  res = readdir_r((DIR*)dirPtr, p, pDirE);
  if (res != 0) {
      *pDirE = NULL;
      free(p);
  }
  else if (*pDirE == NULL) {
    // end of stream
    free(p);
  }
  return res;
#else

  if (pDirE == NULL) {
    return -1;
  }

  *pDirE = readdir((DIR*)dirPtr);
  if (*pDirE == NULL) {
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
HsInt
__hscore_renameFile( HsAddr src,
		     HsAddr dest)
{
#if defined(mingw32_HOST_OS) || defined(__MINGW32__) || defined(_MSC_VER)
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
	errno = toErrno(GetLastError());
	return (-1);
    }

    if (forNT == -1) {
	OSVERSIONINFO ovi;
	ovi.dwOSVersionInfoSize = sizeof(ovi);
	if ( !GetVersionEx(&ovi) ) {
	    errno = toErrno(GetLastError()); 
	    return (-1);
	}
	forNT = ((ovi.dwPlatformId & VER_PLATFORM_WIN32_NT) != 0);
    }
    
    if (forNT) {
	/* Easy, go for MoveFileEx() */
	if ( MoveFileExA(src, dest, MOVEFILE_REPLACE_EXISTING) ) {
	    return 0;
	} else {
	    errno = toErrno(GetLastError()); 
	    return (-1);
	}
    }

    /* No MoveFileEx() for Win9x, try deleting the target. */
    /* Similarly, if the MoveFile*() ops didn't work out under NT */
    if (DeleteFileA(dest)) {
	if (MoveFileA(src,dest)) {
	    return 0;
	} else {
	    errno = toErrno(GetLastError());
	    return (-1);
	}
    } else {
	errno = toErrno(GetLastError());
	return (-1);
    }
#else
    return rename(src,dest);
#endif
}

/*
 * Function: __hscore_getFolderPath()
 *
 * Late-bound version of SHGetFolderPath(), coping with OS versions
 * that have shell32's lacking that particular API.
 *
 */
#if defined(mingw32_HOST_OS) || defined(__MINGW32__) || defined(_MSC_VER)
typedef HRESULT (*HSCORE_GETAPPFOLDERFUNTY)(HWND,int,HANDLE,DWORD,char*);
int
__hscore_getFolderPath(HWND hwndOwner,
		       int nFolder,
		       HANDLE hToken,
		       DWORD dwFlags,
		       char*  pszPath)
{
    static int loaded_dll = 0;
    static HMODULE hMod = (HMODULE)NULL;
    static HSCORE_GETAPPFOLDERFUNTY funcPtr = NULL;
    /* The DLLs to try loading entry point from */
    char* dlls[] = { "shell32.dll", "shfolder.dll" };
    
    if (loaded_dll < 0) {
	return (-1);
    } else if (loaded_dll == 0) {
	int i;
	for(i=0;i < sizeof(dlls); i++) {
	    hMod = LoadLibrary(dlls[i]);
	    if ( hMod != NULL &&
		 (funcPtr = (HSCORE_GETAPPFOLDERFUNTY)GetProcAddress(hMod, "SHGetFolderPathA")) ) {
		loaded_dll = 1;
		break;
	    }
	}
	if (loaded_dll == 0) {
	    loaded_dll = (-1);
	    return (-1);
	}
    }
    /* OK, if we got this far the function has been bound */
    return (int)funcPtr(hwndOwner,nFolder,hToken,dwFlags,pszPath);
    /* ToDo: unload the DLL on shutdown? */
}
#endif
