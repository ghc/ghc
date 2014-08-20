/* -----------------------------------------------------------------------------
 *
 * User-overridable RTS hooks.
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include <stdio.h>

void
StackOverflowHook (W_ stack_size)    /* in bytes */
{
    fprintf(stderr, "Stack space overflow: current size %" FMT_Word " bytes.\nUse `+RTS -Ksize -RTS' to increase it.\n", stack_size);
}


// Local Variables:
// mode: C
// fill-column: 80
// indent-tabs-mode: nil
// c-basic-offset: 4
// buffer-file-coding-system: utf-8-unix
// End:
