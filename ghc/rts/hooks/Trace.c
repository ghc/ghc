/* -----------------------------------------------------------------------------
 * $Id: Trace.c,v 1.4 1999/08/26 08:24:16 panne Exp $
 *
 * User-overridable RTS hooks.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"

void
PreTraceHook (long fd STG_UNUSED)
{
  /* Default is not to print anything, however this might be useful:
   *
   * const char msg[]="\nTrace On:\n";
   * write(fd,msg,sizeof(msg)-1);
   */
}

void
PostTraceHook (long fd STG_UNUSED)
{
  /* Default is not to print anything, however this might be useful:
   *
   * const char msg[]="\nTrace Off.\n";
   * write(fd,msg,sizeof(msg)-1);
   */
}

