/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2006-2009
 *
 * Spin locks
 *
 * These are simple spin-only locks as opposed to Mutexes which
 * probably spin for a while before blocking in the kernel.  We use
 * these when we are sure that all our threads are actively running on
 * a CPU, eg. in the GC.
 *
 * TODO: measure whether we really need these, or whether Mutexes
 * would do (and be a bit safer if a CPU becomes loaded).
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * -------------------------------------------------------------------------- */

#include "PosixSource.h"
#include "Rts.h"

#if defined(THREADED_RTS)

void acquire_spin_lock_slow_path(SpinLock * p)
{
    do {
        for (uint32_t i = 0; i < SPIN_COUNT; i++) {
            StgWord32 r = cas((StgVolatilePtr)&(p->lock), 1, 0);
            if (r != 0) return;
            IF_PROF_SPIN(RELAXED_ADD(&p->spin, 1));
            busy_wait_nop();
        }
        IF_PROF_SPIN(RELAXED_ADD(&p->yield, 1));
        yieldThread();
    } while (1);
}

#endif
