/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2001-2016
 *
 * Platform-independent path manipulation utilities
 *
 * --------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

// Use wchar_t for pathnames on Windows (#5697)
#if defined(mingw32_HOST_OS)
#define pathcmp wcscmp
#define pathlen wcslen
#define pathopen __rts_fwopen
#define pathstat _wstat
#define struct_stat struct _stat
#define open wopen
#define WSTR(s) L##s
#define pathprintf swprintf
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
#endif

pathchar* pathdup(pathchar *path);
pathchar* pathdir(pathchar *path);
pathchar* mkPath(char* path);
HsBool endsWithPath(pathchar* base, pathchar* str);

#include "EndPrivate.h"
