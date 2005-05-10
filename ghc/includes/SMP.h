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
 */
INLINE_HEADER StgWord
xchg(StgPtr p, StgWord w)
{
    StgWord result;
    result = w;
    __asm__ __volatile__ (
 	  "xchgl %1,%0"
          :"+r" (result), "+m" (*p)
          : /* no input-only operands */
	);
    return result;
}

INLINE_HEADER StgInfoTable *
lockClosure(StgClosure *p)
{
    StgWord info;
#if 0
    do {
	info = xchg((P_)&p->header.info, (W_)&stg_WHITEHOLE_info);
	if (info != (W_)&stg_WHITEHOLE_info) return (StgInfoTable *)info;
	yieldThread();
    } while (1);
#else
    info = p->header.info;
#endif
}

#endif /* SMP */

#endif /* SMP_H */
