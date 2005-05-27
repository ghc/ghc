/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2005
 *
 * Macros for SMP support
 *
 * -------------------------------------------------------------------------- */

#ifndef SMP_H
#define SMP_H

/* SMP is currently not compatible with the following options:
 *
 *      INTERPRETER
 *      PROFILING
 *      TICKY_TICKY
 *      and unregisterised builds.
 */

#if defined(SMP)

#if  defined(PROFILING)  || defined(TICKY_TICKY)
#error Build options incompatible with SMP.
#endif

/* 
 * XCHG - the atomic exchange instruction.  Used for locking closures
 * during updates (see LOCK_CLOSURE below) and the MVar primops.
 *
 * NB: the xchg instruction is implicitly locked, so we do not need
 * a lock prefix here. 
 */
INLINE_HEADER StgWord
xchg(StgPtr p, StgWord w)
{
    StgWord result;
    result = w;
    __asm__ __volatile__ (
 	  "xchg %1,%0"
          :"+r" (result), "+m" (*p)
          : /* no input-only operands */
	);
    return result;
}

/* 
 * CMPXCHG - the single-word atomic compare-and-exchange instruction.  Used 
 * in the STM implementation.
 */
INLINE_HEADER StgWord
cas(StgVolatilePtr p, StgWord o, StgWord n)
{
    __asm__ __volatile__ (
 	  "lock cmpxchg %3,%1"
          :"=a"(o), "=m" (*(volatile unsigned int *)p) 
          :"0" (o), "r" (n));
    return o;
}

INLINE_HEADER StgInfoTable *
lockClosure(StgClosure *p)
{
#if i386_HOST_ARCH || x86_64_HOST_ARCH
    StgWord info;
    do {
	info = xchg((P_)&p->header.info, (W_)&stg_WHITEHOLE_info);
	if (info != (W_)&stg_WHITEHOLE_info) return (StgInfoTable *)info;
	yieldThread();
    } while (1);
#else
   ACQUIRE_SM_LOCK
#endif
}

INLINE_HEADER void
unlockClosure(StgClosure *p, StgInfoTable *info)
{
#if i386_HOST_ARCH || x86_64_HOST_ARCH
    // This is safe enough, because lockClosure() does the memory barrier:
    p->header.info = info;
#else
    RELEASE_SM_LOCK;
#endif
}

#endif /* SMP */

#endif /* SMP_H */
