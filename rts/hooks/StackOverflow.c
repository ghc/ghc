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
    fprintf(stderr, "Stack space overflow: current size %" FMT_SizeT " bytes.\nUse `+RTS -Ksize -RTS' to increase it.\n", stack_size);
}

