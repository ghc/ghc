/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * General utility functions used in the RTS.
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTSUTILS_H
#define RTSUTILS_H

#include "BeginPrivate.h"

/* -----------------------------------------------------------------------------
 * (Checked) dynamic allocation
 * -------------------------------------------------------------------------- */

void initAllocator(void);
void shutdownAllocator(void);

void *stgMallocBytes(int n, char *msg)
    GNUC3_ATTRIBUTE(__malloc__);

void *stgReallocBytes(void *p, int n, char *msg);

void *stgCallocBytes(int n, int m, char *msg)
     GNUC3_ATTRIBUTE(__malloc__);

void stgFree(void* p);

/* -----------------------------------------------------------------------------
 * Misc other utilities
 * -------------------------------------------------------------------------- */

void heapOverflow(void);

char *time_str(void);
char *showStgWord64(StgWord64, char *, rtsBool);

#ifdef DEBUG
void heapCheckFail( void );
#endif

void printRtsInfo(void);

void checkFPUStack(void);

#include "EndPrivate.h"

#endif /* RTSUTILS_H */
