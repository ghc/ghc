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

/* 
 * XCHG - the atomic exchange instruction.  Used for locking closures
 * during updates (see lockClosure() below) and the MVar primops.
 *
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
        :"=r" (result)
        :"r" (o), "r" (n), "r" (p)
    );
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
#else
#error memory barriers unimplemented on this architecture
#endif
}

/*
 * Locking/unlocking closures
 *
 * This is used primarily in the implementation of MVars.
 */
#define SPIN_COUNT 4000

INLINE_HEADER StgInfoTable *
lockClosure(StgClosure *p)
{
#if i386_HOST_ARCH || x86_64_HOST_ARCH || powerpc_HOST_ARCH
    StgWord info;
    do {
	nat i = 0;
	do {
	    info = xchg((P_)(void *)&p->header.info, (W_)&stg_WHITEHOLE_info);
	    if (info != (W_)&stg_WHITEHOLE_info) return (StgInfoTable *)info;
	} while (++i < SPIN_COUNT);
	yieldThread();
    } while (1);
#else
   ACQUIRE_SM_LOCK
#endif
}

INLINE_HEADER void
unlockClosure(StgClosure *p, StgInfoTable *info)
{
#if i386_HOST_ARCH || x86_64_HOST_ARCH || powerpc_HOST_ARCH
    // This is a strictly ordered write, so we need a wb():
    write_barrier();
    p->header.info = info;
#else
    RELEASE_SM_LOCK;
#endif
}

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

#endif /* !THREADED_RTS */

// Handy specialised versions of lockClosure()/unlockClosure()
INLINE_HEADER void lockTSO(StgTSO *tso)
{ lockClosure((StgClosure *)tso); }

INLINE_HEADER void unlockTSO(StgTSO *tso)
{ unlockClosure((StgClosure*)tso, (StgInfoTable*)&stg_TSO_info); }

#endif /* SMP_H */
