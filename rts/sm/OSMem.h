/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2006-2008
 *
 * OS-specific memory management
 *
 * ---------------------------------------------------------------------------*/

#ifndef SM_OSMEM_H
#define SM_OSMEM_H

BEGIN_RTS_PRIVATE

void osMemInit(void);
void *osGetMBlocks(nat n);
void osFreeAllMBlocks(void);
lnat getPageSize (void);
void setExecutable (void *p, lnat len, rtsBool exec);

END_RTS_PRIVATE

#endif /* SM_OSMEM_H */
