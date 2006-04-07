/* -----------------------------------------------------------------------------
 *
 * User-overridable RTS hooks.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"

#include <stdio.h>

void
StackOverflowHook (lnat stack_size)    /* in bytes */
{
    fprintf(stderr, "Stack space overflow: current size %ld bytes.\nUse `+RTS -Ksize' to increase it.\n", stack_size);
}

