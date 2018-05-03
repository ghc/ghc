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

void *stgMallocBytes(size_t n, char *msg)
    GNUC3_ATTRIBUTE(__malloc__);

void *stgReallocBytes(void *p, size_t n, char *msg);

void *stgCallocBytes(size_t n, size_t m, char *msg)
     GNUC3_ATTRIBUTE(__malloc__);

char *stgStrndup(const char *s, size_t n);

void stgFree(void* p);

/* -----------------------------------------------------------------------------
 * Misc other utilities
 * -------------------------------------------------------------------------- */

char *time_str(void);
char *showStgWord64(StgWord64, char *, bool);

#if defined(DEBUG)
void heapCheckFail( void );
#endif

void printRtsInfo(void);

void checkFPUStack(void);

#define xstr(s) str(s)
#define str(s) #s

#include "EndPrivate.h"
