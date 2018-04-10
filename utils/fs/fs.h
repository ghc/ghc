/* -----------------------------------------------------------------------------
 *
 * (c) Tamar Christina 2018
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

int FS(swopen) (const wchar_t* filename, int oflag,
                int shflag, int pmode);
FILE *FS(fwopen) (const wchar_t* filename, const wchar_t* mode);
FILE *FS(fopen) (const char* filename, const char* mode);
#else

FILE *FS(fopen) (const char* filename, const char* mode);
#endif
