/* -----------------------------------------------------------------------------
 * $Id: ErrorHdr.c,v 1.3 2002/07/17 09:21:51 simonmar Exp $
 *
 * User-overridable RTS hooks.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

void
ErrorHdrHook (long fd)
{
    const char msg[] = "\nFail: ";
    write(fd, msg, sizeof(msg)-1);
}
