/* -----------------------------------------------------------------------------
 *
 * User-overridable RTS hooks.
 *
 * ---------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"
#include "Hooks.h"
#include "RtsFlags.h"

#include <stdio.h>

void
StackOverflowHook (W_ stack_size)    /* in bytes */
{
    errorBelch("Stack space overflow: current size %" FMT_Word " bytes.",
               stack_size);

    if (rtsConfig.rts_opts_suggestions == true) {
        if (rtsConfig.rts_opts_enabled == RtsOptsAll) {
            errorBelch("Use `+RTS -Ksize -RTS' to increase it.");
        } else {
            errorBelch("Relink with -rtsopts and "
                       "use `+RTS -Ksize -RTS' to increase it.");
        }
    }
}
