/* ----------------------------------------------------------------------------
 * $Id: SMP.h,v 1.5 2005/01/28 12:55:52 simonmar Exp $
 *
 * (c) The GHC Team, 1999
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
 * CMPXCHG - this instruction is the standard "test & set".  We use it
 * for locking closures in the thunk and blackhole entry code.  If the
 * closure is already locked, or has an unexpected info pointer
 * (because another thread is altering it in parallel), we just jump
 * to the new entry point.
 */
#if defined(i386_HOST_ARCH) && defined(TABLES_NEXT_TO_CODE)
#define CMPXCHG(p, cmp, new)			\
  __asm__ __volatile__ (  			\
	  "lock ; cmpxchg %1, %0\n"		\
          "\tje 1f\n"				\
          "\tjmp *%%eax\n"			\
          "\t1:\n"				\
	  : /* no outputs */			\
	  : "m" (p), "r" (new), "r" (cmp)	\
	  )

/* 
 * XCHG - the atomic exchange instruction.  Used for locking closures
 * during updates (see LOCK_CLOSURE below) and the MVar primops.
 */
#define XCHG(reg, obj)				\
  __asm__ __volatile__ (			\
 	  "xchgl %1,%0"				\
          :"+r" (reg), "+m" (obj)		\
          : /* no input-only operands */	\
	  )

#else
#error SMP macros not defined for this architecture
#endif

/*
 * LOCK_CLOSURE locks the specified closure, busy waiting for any
 * existing locks to be cleared.
 */
#define LOCK_CLOSURE(c)					\
  ({							\
    const StgInfoTable *__info;				\
    __info = &stg_WHITEHOLE_info;			\
    do {						\
      XCHG(__info,((StgClosure *)(c))->header.info);	\
    } while (__info == &stg_WHITEHOLE_info);		\
    __info;						\
  })

#define LOCK_THUNK(__info)				\
  CMPXCHG(R1.cl->header.info, __info, &stg_WHITEHOLE_info);

#else /* !SMP */

#define LOCK_CLOSURE(c)     /* nothing */
#define LOCK_THUNK(__info)  /* nothing */

#endif /* SMP */

#endif /* SMP_H */
