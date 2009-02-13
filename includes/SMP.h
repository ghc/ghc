/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2005-2008
 *
 * Macros for multi-CPU support
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
   
#if !IN_STG_CODE
// We only want write_barrier() declared in .hc files.  Defining the
// other inline functions here causes type mismatch errors from gcc,
// because the generated C code is assuming that there are no
// prototypes in scope.

/* 
 * The atomic exchange operation: xchg(p,w) exchanges the value
 * pointed to by p with the value w, returning the old value.
 *
 * Used for locking closures during updates (see lockClosure() below)
 * and the MVar primops.
 */
EXTERN_INLINE StgWord xchg(StgPtr p, StgWord w);

/* 
 * Compare-and-swap.  Atomically does this:
 *
 * cas(p,o,n) { 
 *    r = *p; 
 *    if (r == o) { *p = n }; 
 *    return r;
 * }
 */
EXTERN_INLINE StgWord cas(StgVolatilePtr p, StgWord o, StgWord n);

#endif // !IN_STG_CODE

/*
 * Various kinds of memory barrier.
 *  write_barrier: prevents future stores occurring before prededing stores.
 *  store_load_barrier: prevents future loads occurring before preceding stores.
 *  load_load_barrier: prevents future loads occurring before earlier stores.
 *
 * Reference for these: "The JSR-133 Cookbook for Compiler Writers"
 * http://gee.cs.oswego.edu/dl/jmm/cookbook.html
 *
 * To check whether you got these right, try the test in 
 *   testsuite/tests/ghc-regress/rts/testwsdeque.c
 * This tests the work-stealing deque implementation, which relies on
 * properly working store_load and load_load memory barriers.
 */ 
EXTERN_INLINE void write_barrier(void);
EXTERN_INLINE void store_load_barrier(void);
EXTERN_INLINE void load_load_barrier(void);

/* ----------------------------------------------------------------------------
   Implementations
   ------------------------------------------------------------------------- */

#if !IN_STG_CODE

/* 
 * NB: the xchg instruction is implicitly locked, so we do not need
 * a lock prefix here. 
 */
EXTERN_INLINE StgWord
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
        :"=&r" (result)
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
EXTERN_INLINE StgWord
cas(StgVolatilePtr p, StgWord o, StgWord n)
{
#if i386_HOST_ARCH || x86_64_HOST_ARCH
    __asm__ __volatile__ (
 	  "lock\ncmpxchg %3,%1"
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

#endif // !IN_STG_CODE

/*
 * We need to tell both the compiler AND the CPU about the barriers.
 * It's no good preventing the CPU from reordering the operations if
 * the compiler has already done so - hence the "memory" restriction
 * on each of the barriers below.
 */
EXTERN_INLINE void
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

EXTERN_INLINE void
store_load_barrier(void) {
#if i386_HOST_ARCH
    __asm__ __volatile__ ("lock; addl $0,0(%%esp)" : : : "memory");
#elif x86_64_HOST_ARCH
    __asm__ __volatile__ ("lock; addq $0,0(%%rsp)" : : : "memory");
#elif powerpc_HOST_ARCH
    __asm__ __volatile__ ("sync" : : : "memory");
#elif sparc_HOST_ARCH
    /* Sparc in TSO mode does not require write/write barriers. */
    __asm__ __volatile__ ("membar" : : : "memory");
#elif !defined(WITHSMP)
    return;
#else
#error memory barriers unimplemented on this architecture
#endif
}

EXTERN_INLINE void
load_load_barrier(void) {
#if i386_HOST_ARCH
    __asm__ __volatile__ ("" : : : "memory");
#elif x86_64_HOST_ARCH
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

/* ---------------------------------------------------------------------- */
#else /* !THREADED_RTS */

#define write_barrier()      /* nothing */
#define store_load_barrier() /* nothing */
#define load_load_barrier()  /* nothing */

INLINE_HEADER StgWord
xchg(StgPtr p, StgWord w)
{
    StgWord old = *p;
    *p = w;
    return old;
}

STATIC_INLINE StgWord
cas(StgVolatilePtr p, StgWord o, StgWord n)
{
    StgWord result;
    result = *p;
    if (result == o) {
        *p = n;
    }
    return result;
}

#endif /* !THREADED_RTS */

#endif /* SMP_H */
