#include "../../../includes/ghcconfig.h"

#include "HsFFI.h"

#if HAVE_LIMITS_H
#include <limits.h>
#endif
#if HAVE_WINDOWS_H
#include <windows.h>
#endif

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
#endif
