/* -----------------------------------------------------------------------------
 *
 * User-overridable RTS hooks.
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "sm/GC.h"
#include "sm/GCThread.h"
#include "Hooks.h"

/*
 * Called when --long-gc-sync=<time> has expired during a GC sync.  The idea is
 * that you can set a breakpoint on this function in gdb and try to determine
 * which thread was holding up the GC sync.
 */
void LongGCSync (uint32_t me USED_IF_THREADS, Time t STG_UNUSED)
{
#if defined(THREADED_RTS)
    {
        uint32_t i;
        for (i=0; i < n_capabilities; i++) {
            if (i != me && gc_threads[i]->wakeup != GC_THREAD_STANDING_BY) {
                debugBelch("Warning: slow GC sync: still waiting for cap %d\n",
                           i);
            }
        }
    }
#endif
}

/*
 * Called at the end of a GC sync which was longer than --long-gc-sync=<time>.
 * The idea is that you can use this function to log stats about the length of
 * GC syncs.
 */
void LongGCSyncEnd (Time t)
{
    debugBelch("Warning: waited %" FMT_Word64 "us for GC sync\n", TimeToUS(t));
}
