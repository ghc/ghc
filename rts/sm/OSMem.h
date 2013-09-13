/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2006-2008
 *
 * OS-specific memory management
 *
 * ---------------------------------------------------------------------------*/

#ifndef SM_OSMEM_H
#define SM_OSMEM_H

#include "BeginPrivate.h"

void osMemInit(void);
void *osGetMBlocks(nat n);
void osFreeMBlocks(char *addr, nat n);
void osReleaseFreeMemory(void);
void osFreeAllMBlocks(void);
W_ getPageSize (void);
StgWord64 getPhysicalMemorySize (void);
void setExecutable (void *p, W_ len, rtsBool exec);

#include "EndPrivate.h"

#endif /* SM_OSMEM_H */
