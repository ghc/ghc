/* -----------------------------------------------------------------------------
 * $Id: ErrorHdr.c,v 1.2 1998/12/02 13:29:11 simonm Exp $
 *
 * User-overridable RTS hooks.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"

void
ErrorHdrHook (long fd)
{
    const char msg[] = "\nFail: ";
    write(fd, msg, sizeof(msg)-1);
}
