#include "../../../includes/ghcconfig.h"

#include "HsFFI.h"

#if HAVE_LIMITS_H
#include <limits.h>
#endif
#if HAVE_WINDOWS_H
#include <windows.h>
#endif
#include "directory.h"

#define INLINE /* nothing */

/*
 * Following code copied from libraries/base/includes/HsBase.h
 */

#ifdef PATH_MAX
/* A size that will contain many path names, but not necessarily all
 * (PATH_MAX is not defined on systems with unlimited path length,
 * e.g. the Hurd).
 */
INLINE HsInt __compat_long_path_size() { return PATH_MAX; } 
#else
INLINE HsInt __compat_long_path_size() { return 4096; }
#endif

#if defined(mingw32_HOST_OS)

/* Make sure we've got the reqd CSIDL_ constants in scope;
 * w32api header files are lagging a bit in defining the full set.
 */
#if !defined(CSIDL_APPDATA)
#define CSIDL_APPDATA 0x001a
#endif
#if !defined(CSIDL_PERSONAL)
#define CSIDL_PERSONAL 0x0005
#endif
#if !defined(CSIDL_PROFILE)
#define CSIDL_PROFILE 0x0028
#endif
#if !defined(CSIDL_WINDOWS)
#define CSIDL_WINDOWS 0x0024
#endif

INLINE int __hscore_CSIDL_PROFILE()  { return CSIDL_PROFILE;  }
INLINE int __hscore_CSIDL_APPDATA()  { return CSIDL_APPDATA;  }
INLINE int __hscore_CSIDL_WINDOWS()  { return CSIDL_WINDOWS;  }
INLINE int __hscore_CSIDL_PERSONAL() { return CSIDL_PERSONAL; }

/*
 * Function: __hscore_getFolderPath()
 *
 * Late-bound version of SHGetFolderPath(), coping with OS versions
 * that have shell32's lacking that particular API.
 *
 */
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
    
    if (loaded_dll < 0) {
	return (-1);
    } else if (loaded_dll == 0) {
	hMod = LoadLibrary("shell32.dll");
	if (hMod == NULL) {
	    loaded_dll = (-1);
	    return (-1);
	} else {
	    funcPtr = (HSCORE_GETAPPFOLDERFUNTY)GetProcAddress(hMod, "SHGetFolderPathA");
	    if (!funcPtr) {
		loaded_dll = (-1);
		return (-1);
	    } else {
		loaded_dll = 1;
	    }
	}
    }
    /* OK, if we got this far the function has been bound */
    return (int)funcPtr(hwndOwner,nFolder,hToken,dwFlags,pszPath);
    /* ToDo: unload the DLL? */
}
#endif
