/* -----------------------------------------------------------------------------
 * $Id: MallocFail.c,v 1.3 2002/07/17 09:21:51 simonmar Exp $
 *
 * User-overridable RTS hooks.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"

#include <stdio.h>

void
MallocFailHook (lnat request_size /* in bytes */, char *msg)
{
    fprintf(stderr, "malloc: failed on request for %lu bytes; message: %s\n", request_size, msg);
}

