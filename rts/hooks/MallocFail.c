/* -----------------------------------------------------------------------------
 *
 * User-overridable RTS hooks.
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include <stdio.h>

void
MallocFailHook (lnat request_size /* in bytes */, char *msg)
{
    fprintf(stderr, "malloc: failed on request for %" FMT_SizeT " bytes; message: %s\n", request_size, msg);
}

