/* -----------------------------------------------------------------------------
 *
 * User-overridable RTS hooks.
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "Hooks.h"
#include "RtsFlags.h"

#include <stdio.h>

void
StackOverflowHook (W_ stack_size)    /* in bytes */
{
    fprintf(stderr,
            "Stack space overflow: current size %" FMT_Word " bytes.\n"
            "%s `+RTS -Ksize -RTS' to increase it.\n",
            stack_size,
            ((rtsConfig.rts_opts_enabled == RtsOptsAll)
             ? "Use"
             : "Relink with -rtsopts and use")
            );
}
