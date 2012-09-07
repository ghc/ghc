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
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * -------------------------------------------------------------------------- */

#ifndef RTS_SPINLOCK_H
#define RTS_SPINLOCK_H
 
#if defined(THREADED_RTS)

#if defined(PROF_SPIN)
typedef struct SpinLock_
{
    StgWord   lock;
    StgWord64 spin; // DEBUG version counts how much it spins
} SpinLock;
#else
typedef StgWord SpinLock;
#endif

typedef StgWord SpinLockCount;

#if defined(PROF_SPIN)

// PROF_SPIN enables counting the number of times we spin on a lock

// acquire spin lock
INLINE_HEADER void ACQUIRE_SPIN_LOCK(SpinLock * p)
{
    StgWord32 r = 0;
    nat i;
    do {
        for (i = 0; i < SPIN_COUNT; i++) {
            r = cas((StgVolatilePtr)&(p->lock), 1, 0);
            if (r != 0) return;
            p->spin++;
            busy_wait_nop();
        }
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
    p->spin = 0;
}

#else

// acquire spin lock
INLINE_HEADER void ACQUIRE_SPIN_LOCK(SpinLock * p)
{
    StgWord32 r = 0;
    nat i;
    do {
        for (i = 0; i < SPIN_COUNT; i++) {
            r = cas((StgVolatilePtr)p, 1, 0);
            if (r != 0) return;
            busy_wait_nop();
        }
        yieldThread();
    } while (1);
}

// release spin lock
INLINE_HEADER void RELEASE_SPIN_LOCK(SpinLock * p)
{
    write_barrier();
    (*p) = 1;
}

// init spin lock
INLINE_HEADER void initSpinLock(SpinLock * p)
{
    write_barrier();
    (*p) = 1;
}

#endif /* PROF_SPIN */

#else /* !THREADED_RTS */

// Using macros here means we don't have to ensure the argument is in scope
#define ACQUIRE_SPIN_LOCK(p) /* nothing */
#define RELEASE_SPIN_LOCK(p) /* nothing */

INLINE_HEADER void initSpinLock(void * p STG_UNUSED)
{ /* nothing */ }

#endif /* THREADED_RTS */

#endif /* RTS_SPINLOCK_H */

