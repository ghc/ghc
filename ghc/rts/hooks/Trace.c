/* -----------------------------------------------------------------------------
 * $Id: Trace.c,v 1.3 1999/01/25 12:01:21 simonm Exp $
 *
 * User-overridable RTS hooks.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"

void
PreTraceHook (long fd UNUSED)
{
  /* Default is not to print anything, however this might be useful:
   *
   * const char msg[]="\nTrace On:\n";
   * write(fd,msg,sizeof(msg)-1);
   */
}

void
PostTraceHook (long fd UNUSED)
{
  /* Default is not to print anything, however this might be useful:
   *
   * const char msg[]="\nTrace Off.\n";
   * write(fd,msg,sizeof(msg)-1);
   */
}

