/* -----------------------------------------------------------------------------
 * $Id: StackOverflow.c,v 1.2 1998/12/02 13:29:15 simonm Exp $
 *
 * User-overridable RTS hooks.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"

void
StackOverflowHook (lnat stack_size)    /* in bytes */
{
    fprintf(stderr, "Stack space overflow: current size %ld bytes.\nUse `+RTS -Ksize' to increase it.\n", stack_size);
}

