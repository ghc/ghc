/* -----------------------------------------------------------------------------
 *
 * User-overridable RTS hooks.
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "Hooks.h"

#include <stdio.h>

void
MallocFailHook (W_ request_size /* in bytes */, const char *msg)
{
    fprintf(stderr, "malloc: failed on request for %" FMT_Word " bytes; message: %s\n", request_size, msg);
}

