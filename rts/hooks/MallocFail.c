/* -----------------------------------------------------------------------------
 *
 * User-overridable RTS hooks.
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include <stdio.h>

void
MallocFailHook (W_ request_size /* in bytes */, char *msg)
{
    fprintf(stderr, "malloc: failed on request for %" FMT_Word " bytes; message: %s\n", request_size, msg);
}


// Local Variables:
// mode: C
// fill-column: 80
// indent-tabs-mode: nil
// c-basic-offset: 4
// buffer-file-coding-system: utf-8-unix
// End:
