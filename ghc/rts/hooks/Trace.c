/* -----------------------------------------------------------------------------
 * $Id: Trace.c,v 1.2 1998/12/02 13:29:16 simonm Exp $
 *
 * User-overridable RTS hooks.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"

void
PreTraceHook (long fd)
{
  /* Default is not to print anything, however this might be useful:
   *
   * const char msg[]="\nTrace On:\n";
   * write(fd,msg,sizeof(msg)-1);
   */
}

void
PostTraceHook (long fd)
{
  /* Default is not to print anything, however this might be useful:
   *
   * const char msg[]="\nTrace Off.\n";
   * write(fd,msg,sizeof(msg)-1);
   */
}

