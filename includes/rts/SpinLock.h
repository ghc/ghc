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

#pragma once

#if defined(THREADED_RTS)

typedef struct SpinLock_
{
    StgWord   lock;
#if defined(PROF_SPIN)
    StgWord64 spin;  // incremented every time we spin in ACQUIRE_SPIN_LOCK
    StgWord64 yield; // incremented every time we yield in ACQUIRE_SPIN_LOCK
#endif
} SpinLock;

// PROF_SPIN enables counting the number of times we spin on a lock
#if defined(PROF_SPIN)
#define IF_PROF_SPIN(x) x
#else
#define IF_PROF_SPIN(x)
#endif

// acquire spin lock
INLINE_HEADER void ACQUIRE_SPIN_LOCK(SpinLock * p)
{
    do {
        for (uint32_t i = 0; i < SPIN_COUNT; i++) {
            StgWord32 r = cas((StgVolatilePtr)&(p->lock), 1, 0);
            if (r != 0) return;
            IF_PROF_SPIN(p->spin++);
            busy_wait_nop();
        }
        IF_PROF_SPIN(p->yield++);
        yieldThread();
    } while (1);
}

// release spin lock
INLINE_HEADER void RELEASE_SPIN_LOCK(SpinLock * p)
{
    write_barrier();
    p->lock = 1;
}

// initialise spin lock
INLINE_HEADER void initSpinLock(SpinLock * p)
{
    write_barrier();
    p->lock = 1;
    IF_PROF_SPIN(p->spin = 0);
    IF_PROF_SPIN(p->yield = 0);
}

#else /* !THREADED_RTS */

// Using macros here means we don't have to ensure the argument is in scope
#define ACQUIRE_SPIN_LOCK(p) /* nothing */
#define RELEASE_SPIN_LOCK(p) /* nothing */

INLINE_HEADER void initSpinLock(void * p STG_UNUSED)
{ /* nothing */ }

#endif /* THREADED_RTS */
