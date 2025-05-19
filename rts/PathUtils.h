/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2001-2016
 *
 * Platform-independent path manipulation utilities
 *
 * --------------------------------------------------------------------------*/

#pragma once

// Use wchar_t for pathnames on Windows (#5697)
#if defined(mingw32_HOST_OS)
#include "fs_rts.h"

#define pathcmp wcscmp
#define pathlen wcslen
// N.B. Use the Win32-based file routines from utils/fs.
#define pathopen FS(fwopen)
#define pathstat FS(_wstat)
#define struct_stat struct _stat
#define open wopen
#define WSTR(s) L##s
#define pathprintf snwprintf
#define pathcopy wcscpy
#define pathsize sizeof(wchar_t)
#else
#define pathcmp strcmp
#define pathlen strlen
#define pathopen fopen
#define pathstat stat
#define struct_stat struct stat
#define WSTR(s) s
#define pathprintf snprintf
#define pathsize sizeof(char)
#define pathcopy strcpy
#endif

#include "BeginPrivate.h"

pathchar* pathdup(const pathchar *path);
pathchar* pathdir(const pathchar *path);
pathchar* mkPath(const char* path);
HsBool endsWithPath(const pathchar* base, const pathchar* str);

#include "EndPrivate.h"
