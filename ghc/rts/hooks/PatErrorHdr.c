/* -----------------------------------------------------------------------------
 * $Id: PatErrorHdr.c,v 1.2 1998/12/02 13:29:15 simonm Exp $
 *
 * User-overridable RTS hooks.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"

void
PatErrorHdrHook (long fd)
{
    const char msg[] = "\nFail: ";
    write(fd,msg,sizeof(msg)-1);
}

