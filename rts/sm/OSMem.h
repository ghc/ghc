/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2006-2008
 *
 * OS-specific memory management
 *
 * ---------------------------------------------------------------------------*/

#ifndef SM_OSMEM_H
#define SM_OSMEM_H

#pragma GCC visibility push(hidden)

void osMemInit(void);
void *osGetMBlocks(nat n);
void osFreeAllMBlocks(void);
lnat getPageSize (void);
void setExecutable (void *p, lnat len, rtsBool exec);

#pragma GCC visibility pop

#endif /* SM_OSMEM_H */
