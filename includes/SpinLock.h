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

#if defined(DEBUG)
typedef struct StgSync_
{
    StgWord32 lock;
    StgWord64 spin; // DEBUG version counts how much it spins
} StgSync;
#else
typedef StgWord StgSync;
#endif

typedef lnat StgSyncCount;


#if defined(DEBUG)

// Debug versions of spin locks maintain a spin count

// How to use: 
//  To use the debug veriosn of the spin locks, a debug version of the program 
//  can be run under a deugger with a break point on stat_exit. At exit time 
//  of the program one can examine the state the spin count counts of various
//  spin locks to check for contention. 

// acquire spin lock
INLINE_HEADER void ACQUIRE_SPIN_LOCK(StgSync * p)
{
    StgWord32 r = 0;
    do {
        p->spin++;
        r = cas((StgVolatilePtr)&(p->lock), 1, 0);
    } while(r == 0);
    p->spin--;
}

// release spin lock
INLINE_HEADER void RELEASE_SPIN_LOCK(StgSync * p)
{
    write_barrier();
    p->lock = 1;
}

// initialise spin lock
INLINE_HEADER void initSpinLock(StgSync * p)
{
    write_barrier();
    p->lock = 1;
    p->spin = 0;
}

#else

// acquire spin lock
INLINE_HEADER void ACQUIRE_SPIN_LOCK(StgSync * p)
{
    StgWord32 r = 0;
    do {
        r = cas((StgVolatilePtr)p, 1, 0);
    } while(r == 0);
}

// release spin lock
INLINE_HEADER void RELEASE_SPIN_LOCK(StgSync * p)
{
    write_barrier();
    (*p) = 1;
}

// init spin lock
INLINE_HEADER void initSpinLock(StgSync * p)
{
    write_barrier();
    (*p) = 1;
}

#endif /* DEBUG */

#else /* !THREADED_RTS */

// Using macros here means we don't have to ensure the argument is in scope
#define ACQUIRE_SPIN_LOCK(p) /* nothing */
#define RELEASE_SPIN_LOCK(p) /* nothing */

INLINE_HEADER void initSpinLock(void * p STG_UNUSED)
{ /* nothing */ }

#endif /* THREADED_RTS */

#endif /* SPINLOCK_H */

