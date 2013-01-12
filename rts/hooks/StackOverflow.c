/* -----------------------------------------------------------------------------
 *
 * User-overridable RTS hooks.
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"

#include <stdio.h>

void
StackOverflowHook (lnat stack_size)    /* in bytes */
{
    fprintf(stderr, "Stack space overflow: current size %" FMT_Word " bytes.\nUse `+RTS -Ksize -RTS' to increase it.\n", stack_size);
}

