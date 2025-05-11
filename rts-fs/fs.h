/* -----------------------------------------------------------------------------
 *
 * (c) Tamar Christina 2018-2019
 *
 * Windows I/O routines for file opening.
 *
 * NOTE: Only modify this file in utils/fs/ and rerun configure. Do not edit
 *       this file in any other directory as it will be overwritten.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include <stdio.h>

#if !defined(FS_NAMESPACE)
#define FS_NAMESPACE hs
#endif

/* Play some dirty tricks to get CPP to expand correctly.  */
#define FS_FULL(ns, name) __##ns##_##name
#define prefix FS_NAMESPACE
#define FS_L(p, n) FS_FULL(p, n)
#define FS(name) FS_L(prefix, name)

#if defined(_WIN32)
#include <wchar.h>
// N.B. <sys/stat.h> defines some macro rewrites to, e.g., turn _wstat into
// _wstat64i32. We must include it here to ensure tat this rewrite applies to
// both the definition and use sites.
#include <sys/types.h>
#include <sys/stat.h>

wchar_t* FS(create_device_name) (const wchar_t*);
int FS(translate_mode) (const wchar_t*);
int FS(swopen) (const wchar_t* filename, int oflag,
                int shflag, int pmode);
int FS(sopen) (const char* filename, int oflag,
               int shflag, int pmode);
FILE *FS(fwopen) (const wchar_t* filename, const wchar_t* mode);
FILE *FS(fopen) (const char* filename, const char* mode);
int FS(_stat) (const char *path, struct _stat *buffer);
int FS(_stat64) (const char *path, struct __stat64 *buffer);
int FS(_wstat) (const wchar_t *path, struct _stat *buffer);
int FS(_wstat64) (const wchar_t *path, struct __stat64 *buffer);
int FS(_wrename) (const wchar_t *from, const wchar_t *to);
int FS(rename) (const char *from, const char *to);
int FS(unlink) (const char *filename);
int FS(_unlink) (const char *filename);
int FS(_wunlink) (const wchar_t *filename);
int FS(remove) (const char *path);
int FS(_wremove) (const wchar_t *path);
#else
FILE *FS(fopen) (const char* filename, const char* mode);
#endif
