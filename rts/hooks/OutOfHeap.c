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
OutOfHeapHook (W_ request_size, W_ heap_size) /* both sizes in bytes */
{
    (void)request_size;   /* keep gcc -Wall happy */
    if (heap_size > 0) {
        errorBelch("Heap exhausted;");
        errorBelch("Current maximum heap size is %" FMT_Word
                   " bytes (%" FMT_Word " MB).",
                   heap_size, heap_size / (1024*1024));

        if (rtsConfig.rts_opts_suggestions == true) {

            if (rtsConfig.rts_opts_enabled == RtsOptsAll) {
                errorBelch("Use `+RTS -M<size>' to increase it.");
            } else {
                errorBelch("Relink with -rtsopts and "
                           "use `+RTS -M<size>' to increase it.");
            }

        }
    } else {
        errorBelch("Out of memory\n");
    }
}
