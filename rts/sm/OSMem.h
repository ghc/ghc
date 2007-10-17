/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2006-2007
 *
 * OS-specific memory management
 *
 * ---------------------------------------------------------------------------*/

void osMemInit(void);
void *osGetMBlocks(nat n);
void osFreeAllMBlocks(void);
lnat getPageSize (void);
void setExecutable (void *p, lnat len, rtsBool exec);
