/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2006
 *
 * OS-specific memory management
 *
 * ---------------------------------------------------------------------------*/

lnat getPageSize (void);
void setExecutable (void *p, lnat len, rtsBool exec);
