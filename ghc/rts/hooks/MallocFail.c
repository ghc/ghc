/* -----------------------------------------------------------------------------
 * $Id: MallocFail.c,v 1.2 1998/12/02 13:29:12 simonm Exp $
 *
 * User-overridable RTS hooks.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"

void
MallocFailHook (lnat request_size /* in bytes */, char *msg)
{
    fprintf(stderr, "malloc: failed on request for %lu bytes; message: %s\n", request_size, msg);
}

