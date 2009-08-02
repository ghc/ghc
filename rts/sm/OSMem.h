/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2006-2008
 *
 * OS-specific memory management
 *
 * ---------------------------------------------------------------------------*/

#ifndef SM_OSMEM_H
#define SM_OSMEM_H

void osMemInit(void);
void *osGetMBlocks(nat n);
void osFreeAllMBlocks(void);
lnat getPageSize (void);
void setExecutable (void *p, lnat len, rtsBool exec);

#endif /* SM_OSMEM_H */
