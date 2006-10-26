/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2005
 *
 * Macros for THREADED_RTS support
 *
 * -------------------------------------------------------------------------- */

#ifndef SMP_H
#define SMP_H

/* THREADED_RTS is currently not compatible with the following options:
 *
 *      PROFILING (but only 1 CPU supported)
 *      TICKY_TICKY
 *      Unregisterised builds are ok, but only 1 CPU supported.
 */

#if defined(THREADED_RTS)

#if  defined(TICKY_TICKY)
#error Build options incompatible with THREADED_RTS.
#endif

/* ----------------------------------------------------------------------------
   Atomic operations
   ------------------------------------------------------------------------- */
   
/* 
 * The atomic exchange operation: xchg(p,w) exchanges the value
 * pointed to by p with the value w, returning the old value.
 *
 * Used for locking closures during updates (see lockClosure() below)
 * and the MVar primops.
 */
INLINE_HEADER StgWord xchg(StgPtr p, StgWord w);

/* 
 * Compare-and-swap.  Atomically does this:
 *
 * cas(p,o,n) { 
 *    r = *p; 
 *    if (r == o) { *p = n }; 
 *    return r;
 * }
 */
INLINE_HEADER StgWord cas(StgVolatilePtr p, StgWord o, StgWord n);

/*
 * Prevents write operations from moving across this call in either
 * direction.
 */ 
INLINE_HEADER void write_barrier(void);

/* ----------------------------------------------------------------------------
   Implementations
   ------------------------------------------------------------------------- */
/* 
 * NB: the xchg instruction is implicitly locked, so we do not need
 * a lock prefix here. 
 */
INLINE_HEADER StgWord
xchg(StgPtr p, StgWord w)
{
    StgWord result;
#if i386_HOST_ARCH || x86_64_HOST_ARCH
    result = w;
    __asm__ __volatile__ (
 	  "xchg %1,%0"
          :"+r" (result), "+m" (*p)
          : /* no input-only operands */
	);
#elif powerpc_HOST_ARCH
    __asm__ __volatile__ (
        "1:     lwarx     %0, 0, %2\n"
        "       stwcx.    %1, 0, %2\n"
        "       bne-      1b"
        :"=r" (result)
        :"r" (w), "r" (p)
    );
#elif sparc_HOST_ARCH
    result = w;
    __asm__ __volatile__ (
        "swap %1,%0"
	: "+r" (result), "+m" (*p)
	: /* no input-only operands */
      );
#elif !defined(WITHSMP)
    result = *p;
    *p = w;
#else
#error xchg() unimplemented on this architecture
#endif
    return result;
}

/* 
 * CMPXCHG - the single-word atomic compare-and-exchange instruction.  Used 
 * in the STM implementation.
 */
INLINE_HEADER StgWord
cas(StgVolatilePtr p, StgWord o, StgWord n)
{
#if i386_HOST_ARCH || x86_64_HOST_ARCH
    __asm__ __volatile__ (
 	  "lock/cmpxchg %3,%1"
          :"=a"(o), "=m" (*(volatile unsigned int *)p) 
          :"0" (o), "r" (n));
    return o;
#elif powerpc_HOST_ARCH
    StgWord result;
    __asm__ __volatile__ (
        "1:     lwarx     %0, 0, %3\n"
        "       cmpw      %0, %1\n"
        "       bne       2f\n"
        "       stwcx.    %2, 0, %3\n"
        "       bne-      1b\n"
        "2:"
        :"=&r" (result)
        :"r" (o), "r" (n), "r" (p)
        :"cc", "memory"
    );
    return result;
#elif sparc_HOST_ARCH
    __asm__ __volatile__ (
	"cas [%1], %2, %0"
	: "+r" (n)
	: "r" (p), "r" (o)
	: "memory"
    );
    return n;
#elif !defined(WITHSMP)
    StgWord result;
    result = *p;
    if (result == o) {
        *p = n;
    }
    return result;
#else
#error cas() unimplemented on this architecture
#endif
}

/*
 * Write barrier - ensure that all preceding writes have happened
 * before all following writes.  
 *
 * We need to tell both the compiler AND the CPU about the barrier.
 * This is a brute force solution; better results might be obtained by
 * using volatile type declarations to get fine-grained ordering
 * control in C, and optionally a memory barrier instruction on CPUs
 * that require it (not x86 or x86_64).
 */
INLINE_HEADER void
write_barrier(void) {
#if i386_HOST_ARCH || x86_64_HOST_ARCH
    __asm__ __volatile__ ("" : : : "memory");
#elif powerpc_HOST_ARCH
    __asm__ __volatile__ ("lwsync" : : : "memory");
#elif sparc_HOST_ARCH
    /* Sparc in TSO mode does not require write/write barriers. */
    __asm__ __volatile__ ("" : : : "memory");
#elif !defined(WITHSMP)
    return;
#else
#error memory barriers unimplemented on this architecture
#endif
}

/* -----------------------------------------------------------------------------
 * Locking/unlocking closures
 *
 * This is used primarily in the implementation of MVars.
 * -------------------------------------------------------------------------- */

#define SPIN_COUNT 4000

INLINE_HEADER StgInfoTable *
lockClosure(StgClosure *p)
{
    StgWord info;
    do {
	nat i = 0;
	do {
	    info = xchg((P_)(void *)&p->header.info, (W_)&stg_WHITEHOLE_info);
	    if (info != (W_)&stg_WHITEHOLE_info) return (StgInfoTable *)info;
	} while (++i < SPIN_COUNT);
	yieldThread();
    } while (1);
}

INLINE_HEADER void
unlockClosure(StgClosure *p, StgInfoTable *info)
{
    // This is a strictly ordered write, so we need a write_barrier():
    write_barrier();
    p->header.info = info;
}

/* -----------------------------------------------------------------------------
 * Spin locks
 *
 * These are simple spin-only locks as opposed to Mutexes which
 * probably spin for a while before blocking in the kernel.  We use
 * these when we are sure that all our threads are actively running on
 * a CPU, eg. in the GC.
 *
 * TODO: measure whether we really need these, or whether Mutexes
 * would do (and be a bit safer if a CPU becomes loaded).
 * -------------------------------------------------------------------------- */

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

/* ---------------------------------------------------------------------- */
#else /* !THREADED_RTS */

#define write_barrier() /* nothing */

INLINE_HEADER StgWord
xchg(StgPtr p, StgWord w)
{
    StgWord old = *p;
    *p = w;
    return old;
}

INLINE_HEADER StgInfoTable *
lockClosure(StgClosure *p)
{ return (StgInfoTable *)p->header.info; }

INLINE_HEADER void
unlockClosure(StgClosure *p STG_UNUSED, StgInfoTable *info STG_UNUSED)
{ /* nothing */ }

// Using macros here means we don't have to ensure the argument is in scope
#define ACQUIRE_SPIN_LOCK(p) /* nothing */
#define RELEASE_SPIN_LOCK(p) /* nothing */

INLINE_HEADER void initSpinLock(void * p STG_UNUSED)
{ /* nothing */ }

#endif /* !THREADED_RTS */

// Handy specialised versions of lockClosure()/unlockClosure()
INLINE_HEADER void lockTSO(StgTSO *tso)
{ lockClosure((StgClosure *)tso); }

INLINE_HEADER void unlockTSO(StgTSO *tso)
{ unlockClosure((StgClosure*)tso, (StgInfoTable*)&stg_TSO_info); }

#endif /* SMP_H */
