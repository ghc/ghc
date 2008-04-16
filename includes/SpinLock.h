/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2006-2008
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
 * -------------------------------------------------------------------------- */

#ifndef SPINLOCK_H
#define SPINLOCK_H
 
#if defined(THREADED_RTS)

#if defined(PROF_SPIN)
typedef struct SpinLock_
{
    StgWord32 lock;
    StgWord64 spin; // DEBUG version counts how much it spins
} SpinLock;
#else
typedef StgWord SpinLock;
#endif

typedef lnat SpinLockCount;


#if defined(PROF_SPIN)

// PROF_SPIN enables counting the number of times we spin on a lock

// acquire spin lock
INLINE_HEADER void ACQUIRE_SPIN_LOCK(SpinLock * p)
{
    StgWord32 r = 0;
spin:
    r = cas((StgVolatilePtr)&(p->lock), 1, 0);
    if (r == 0) {
        p->spin++;
        goto spin;
    }
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
    do {
        r = cas((StgVolatilePtr)p, 1, 0);
    } while(r == 0);
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

#endif /* SPINLOCK_H */

