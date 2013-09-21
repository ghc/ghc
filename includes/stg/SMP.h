/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2005-2011
 *
 * Macros for multi-CPU support
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * -------------------------------------------------------------------------- */

#ifndef SMP_H
#define SMP_H

#if defined(THREADED_RTS)

#if arm_HOST_ARCH && defined(arm_HOST_ARCH_PRE_ARMv6)
void arm_atomic_spin_lock(void);
void arm_atomic_spin_unlock(void);
#endif

/* ----------------------------------------------------------------------------
   Atomic operations
   ------------------------------------------------------------------------- */
   
#if !IN_STG_CODE || IN_STGCRUN
// We only want the barriers, e.g. write_barrier(), declared in .hc
// files.  Defining the other inline functions here causes type
// mismatch errors from gcc, because the generated C code is assuming
// that there are no prototypes in scope.

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

/*
 * Atomic addition by the provided quantity
 *
 * atomic_inc(p, n) {
 *   return ((*p) += n);
 * }
 */
EXTERN_INLINE StgWord atomic_inc(StgVolatilePtr p, StgWord n);


/*
 * Atomic decrement
 *
 * atomic_dec(p) {
 *   return --(*p);
 * }
 */
EXTERN_INLINE StgWord atomic_dec(StgVolatilePtr p);

/*
 * Busy-wait nop: this is a hint to the CPU that we are currently in a
 * busy-wait loop waiting for another CPU to change something.  On a
 * hypertreaded CPU it should yield to another thread, for example.
 */
EXTERN_INLINE void busy_wait_nop(void);

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
 *   testsuite/tests/rts/testwsdeque.c
 * This tests the work-stealing deque implementation, which relies on
 * properly working store_load and load_load memory barriers.
 */ 
EXTERN_INLINE void write_barrier(void);
EXTERN_INLINE void store_load_barrier(void);
EXTERN_INLINE void load_load_barrier(void);

/* ----------------------------------------------------------------------------
   Implementations
   ------------------------------------------------------------------------- */

#if !IN_STG_CODE || IN_STGCRUN

EXTERN_INLINE StgWord
xchg(StgPtr p, StgWord w)
{
    StgWord result;
#if i386_HOST_ARCH || x86_64_HOST_ARCH
    result = w;
    __asm__ __volatile__ (
        // NB: the xchg instruction is implicitly locked, so we do not
        // need a lock prefix here.
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
#elif arm_HOST_ARCH && defined(arm_HOST_ARCH_PRE_ARMv6)
    __asm__ __volatile__ ("swp %0, %1, [%2]"
                         : "=&r" (result)
                         : "r" (w), "r" (p) : "memory");
#elif arm_HOST_ARCH && !defined(arm_HOST_ARCH_PRE_ARMv6)
    // swp instruction which is used in pre-ARMv6 code above
    // is deprecated in AMRv6 and later. ARM, Ltd. *highly* recommends
    // to use ldrex/strex instruction pair for the same purpose
    // see chapter: Synchronization and semaphores in ARM Architecture
    // Reference manual
    StgWord tmp;
    __asm__ __volatile__ (
                          "1:    ldrex  %0, [%3]\n"
                          "      strex  %1, %2, [%3]\n"
                          "      teq    %1, #1\n"
                          "      beq    1b\n"
#if !defined(arm_HOST_ARCH_PRE_ARMv7)
                          "      dmb\n"
#endif
                          : "=&r" (result), "=&r" (tmp)
                          : "r" (w), "r" (p)
                          : "memory"
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
          :"=a"(o), "+m" (*(volatile unsigned int *)p)
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
#elif arm_HOST_ARCH && defined(arm_HOST_ARCH_PRE_ARMv6)
    StgWord r;
    arm_atomic_spin_lock();
    r  = *p; 
    if (r == o) { *p = n; } 
    arm_atomic_spin_unlock();
    return r;
#elif arm_HOST_ARCH && !defined(arm_HOST_ARCH_PRE_ARMv6)
    StgWord result,tmp;

    __asm__ __volatile__(
        "1:     ldrex   %1, [%2]\n"
        "       mov     %0, #0\n"
        "       teq     %1, %3\n"
        "       it      eq\n"
        "       strexeq %0, %4, [%2]\n"
        "       teq     %0, #1\n"
        "       it      eq\n"
        "       beq     1b\n"
#if !defined(arm_HOST_ARCH_PRE_ARMv7)
        "       dmb\n"
#endif
                : "=&r"(tmp), "=&r"(result)
                : "r"(p), "r"(o), "r"(n)
                : "cc","memory");

    return result;
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

// RRN: Generalized to arbitrary increments to enable fetch-and-add in
// Haskell code (fetchAddIntArray#).
EXTERN_INLINE StgWord
atomic_inc(StgVolatilePtr p, StgWord incr)
{
#if defined(i386_HOST_ARCH) || defined(x86_64_HOST_ARCH)
    StgWord r;
    r = incr;
    __asm__ __volatile__ (
        "lock\nxadd %0,%1":
            "+r" (r), "+m" (*p):
    );
    return r + incr;
#else
    StgWord old, new;
    do {
        old = *p;
        new = old + incr;
    } while (cas(p, old, new) != old);
    return new;
#endif
}

EXTERN_INLINE StgWord
atomic_dec(StgVolatilePtr p)
{
#if defined(i386_HOST_ARCH) || defined(x86_64_HOST_ARCH)
    StgWord r;
    r = (StgWord)-1;
    __asm__ __volatile__ (
        "lock\nxadd %0,%1":
            "+r" (r), "+m" (*p):
    );
    return r-1;
#else
    StgWord old, new;
    do {
        old = *p;
        new = old - 1;
    } while (cas(p, old, new) != old);
    return new;
#endif
}

EXTERN_INLINE void
busy_wait_nop(void)
{
#if defined(i386_HOST_ARCH) || defined(x86_64_HOST_ARCH)
    __asm__ __volatile__ ("rep; nop");
    //
#else
    // nothing
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
    /* Sparc in TSO mode does not require store/store barriers. */
    __asm__ __volatile__ ("" : : : "memory");
#elif arm_HOST_ARCH && defined(arm_HOST_ARCH_PRE_ARMv7)
    __asm__ __volatile__ ("" : : : "memory");
#elif arm_HOST_ARCH && !defined(arm_HOST_ARCH_PRE_ARMv7)
    __asm__ __volatile__ ("dmb  st" : : : "memory");
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
    __asm__ __volatile__ ("membar #StoreLoad" : : : "memory");
#elif arm_HOST_ARCH && !defined(arm_HOST_ARCH_PRE_ARMv7)
    __asm__ __volatile__ ("dmb" : : : "memory");
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
    /* Sparc in TSO mode does not require load/load barriers. */
    __asm__ __volatile__ ("" : : : "memory");
#elif arm_HOST_ARCH && !defined(arm_HOST_ARCH_PRE_ARMv7)
    __asm__ __volatile__ ("dmb" : : : "memory");
#elif !defined(WITHSMP)
    return;
#else
#error memory barriers unimplemented on this architecture
#endif
}

// Load a pointer from a memory location that might be being modified
// concurrently.  This prevents the compiler from optimising away
// multiple loads of the memory location, as it might otherwise do in
// a busy wait loop for example.
#define VOLATILE_LOAD(p) (*((StgVolatilePtr)(p)))

/* ---------------------------------------------------------------------- */
#else /* !THREADED_RTS */

EXTERN_INLINE void write_barrier(void);
EXTERN_INLINE void store_load_barrier(void);
EXTERN_INLINE void load_load_barrier(void);
EXTERN_INLINE void write_barrier     () {} /* nothing */
EXTERN_INLINE void store_load_barrier() {} /* nothing */
EXTERN_INLINE void load_load_barrier () {} /* nothing */

#if !IN_STG_CODE || IN_STGCRUN
INLINE_HEADER StgWord
xchg(StgPtr p, StgWord w)
{
    StgWord old = *p;
    *p = w;
    return old;
}

EXTERN_INLINE StgWord cas(StgVolatilePtr p, StgWord o, StgWord n);
EXTERN_INLINE StgWord
cas(StgVolatilePtr p, StgWord o, StgWord n)
{
    StgWord result;
    result = *p;
    if (result == o) {
        *p = n;
    }
    return result;
}

EXTERN_INLINE StgWord atomic_inc(StgVolatilePtr p, StgWord incr);
EXTERN_INLINE StgWord
atomic_inc(StgVolatilePtr p, StgWord incr)
{
    return ((*p) += incr);
}


INLINE_HEADER StgWord
atomic_dec(StgVolatilePtr p)
{
    return --(*p);
}
#endif

#define VOLATILE_LOAD(p) ((StgWord)*((StgWord*)(p)))

#endif /* !THREADED_RTS */

#endif /* SMP_H */
