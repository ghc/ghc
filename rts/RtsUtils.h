/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * General utility functions used in the RTS.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

/* -----------------------------------------------------------------------------
 * (Checked) dynamic allocation
 * -------------------------------------------------------------------------- */

void initAllocator(void);
void shutdownAllocator(void);

void stgFree(void* p);

void *stgMallocBytes(size_t n, char *msg)
    STG_MALLOC STG_MALLOC1(stgFree)
    STG_ALLOC_SIZE1(1);
/* Note: unlike `stgReallocBytes` and `stgCallocBytes`, `stgMallocBytes` is
 * *not* `STG_RETURNS_NONNULL`, since it will return `NULL` when the requested
 * allocation size is zero.
 *
 * See: https://gitlab.haskell.org/ghc/ghc/-/issues/22380
 */

void *stgReallocBytes(void *p, size_t n, char *msg);

void *stgCallocBytes(size_t count, size_t size, char *msg)
    STG_MALLOC STG_MALLOC1(stgFree)
    STG_ALLOC_SIZE2(1, 2)
    STG_RETURNS_NONNULL;

char *stgStrndup(const char *s, size_t n)
    STG_MALLOC STG_MALLOC1(stgFree);

void *stgMallocAlignedBytes(size_t n, size_t align, char *msg);

void stgFreeAligned(void *p);

/* -----------------------------------------------------------------------------
 * Misc other utilities
 * -------------------------------------------------------------------------- */

int rtsSleep(Time t);
char *time_str(void);
char *showStgWord64(StgWord64, char *, bool);

#if defined(DEBUG)
void heapCheckFail( void );
#endif

void printRtsInfo(const RtsConfig);

void checkFPUStack(void);

#define xstr(s) str(s)
#define str(s) #s

// Drop the given extension from a filepath.
void dropExtension(char *path, const char *extension);

#include "EndPrivate.h"
