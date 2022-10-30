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

void *stgReallocBytes(void *p, size_t n, char *msg)
    STG_MALLOC1(stgFree)
    STG_ALLOC_SIZE1(2);
/* Note: `stgRallocBytes` can *not* be tagged as `STG_MALLOC`
 * since its return value *can* alias an existing pointer (i.e.,
 * the given pointer `p`).
 * See the documentation of the `malloc` attribute in the GCC manual
 * for more information.
 */

void *stgCallocBytes(size_t count, size_t size, char *msg)
    STG_MALLOC STG_MALLOC1(stgFree)
    STG_ALLOC_SIZE2(1, 2);

char *stgStrndup(const char *s, size_t n);

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

#include "EndPrivate.h"
