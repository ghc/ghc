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

#ifdef CMINUSMINUS

#define unlockClosure(ptr,info)                 \
    prim %write_barrier() [];                   \
    StgHeader_info(ptr) = info;    

#else

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
 * Prevents write operations from moving across this call in either
 * direction.
 */ 
EXTERN_INLINE void write_barrier(void);

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
 * Write barrier - ensure that all preceding writes have happened
 * before all following writes.  
 *
 * We need to tell both the compiler AND the CPU about the barrier.
 * This is a brute force solution; better results might be obtained by
 * using volatile type declarations to get fine-grained ordering
 * control in C, and optionally a memory barrier instruction on CPUs
 * that require it (not x86 or x86_64).
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

#endif /* !THREADED_RTS */

#endif /* CMINUSMINUS */

#endif /* SMP_H */
