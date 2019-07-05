/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2005-2011
 *
 * Macros for multi-CPU support
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * -------------------------------------------------------------------------- */

#pragma once

#if defined(arm_HOST_ARCH) && defined(arm_HOST_ARCH_PRE_ARMv6)
void arm_atomic_spin_lock(void);
void arm_atomic_spin_unlock(void);
#endif

#if defined(THREADED_RTS)

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
 * Used for locking closures during updates (see lockClosure()
 * in includes/rts/storage/SMPClosureOps.h) and the MVar primops.
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

/*
 * Note [Heap memory barriers]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *
 * Machines with weak memory ordering semantics have consequences for how
 * closures are observed and mutated. For example, consider a thunk that needs
 * to be updated to an indirection. In order for the indirection to be safe for
 * concurrent observers to enter, said observers must read the indirection's
 * info table before they read the indirectee. Furthermore, the indirectee must
 * be set before the info table pointer. This ensures that if the observer sees
 * an IND info table then the indirectee is valid.
 *
 * When a closure is updated with an indirection, both its info table and its
 * indirectee must be written. With weak memory ordering, these two writes can
 * be arbitrarily reordered, and perhaps even interleaved with other threads'
 * reads and writes (in the absence of memory barrier instructions). Consider
 * this example of a bad reordering:
 *
 * - An updater writes to a closure's info table (INFO_TYPE is now IND).
 * - A concurrent observer branches upon reading the closure's INFO_TYPE as IND.
 * - A concurrent observer reads the closure's indirectee and enters it.
 * - An updater writes the closure's indirectee.
 *
 * Here the update to the indirectee comes too late and the concurrent observer
 * has jumped off into the abyss. Speculative execution can also cause us
 * issues, consider:
 *
 * - an observer is about to case on a value in closure's info table.
 * - the observer speculatively reads one or more of closure's fields.
 * - an updater writes to closure's info table.
 * - the observer takes a branch based on the new info table value, but with the
 *   old closure fields!
 * - the updater writes to the closure's other fields, but its too late.
 *
 * Because of these effects, reads and writes to a closure's info table must be
 * ordered carefully with respect to reads and writes to the closure's other
 * fields, and memory barriers must be placed to ensure that reads and writes
 * occur in program order. Specifically, updates to an already existing closure
 * must follow the following pattern:
 *
 * - Update the closure's (non-info table) fields.
 * - Write barrier.
 * - Update the closure's info table.
 *
 * Observing the fields of an updateable closure (e.g. a THUNK) must follow the
 * following pattern:
 *
 * - Read the closure's info pointer.
 * - Read barrier.
 * - Read the closure's (non-info table) fields.
 *
 * We must also take care when we expose a newly-allocated closure to other cores
 * by writing a pointer to it to some shared data structure (e.g. an MVar#, a Message,
 * or MutVar#). Specifically, we need to ensure that all writes constructing the
 * closure are visible *before* the write exposing the new closure is made visible:
 *
 * - Allocate memory for the closure
 * - Write the closure's info pointer and fields (ordering betweeen this doesn't
 *   matter since the closure isn't yet visible to anyone else).
 * - Write barrier
 * - Make closure visible to other cores
 *
 * Note that thread stacks are inherently thread-local and consequently allocating an
 * object and introducing a reference to it to our stack needs no barrier.
 *
 * There are several ways in which the mutator may make a newly-allocated
 * closure visible to other cores:
 *
 *  - Eager blackholing a THUNK:
 *    This is protected by an explicit write barrier in the eager blackholing
 *    code produced by the codegen. See StgCmmBind.emitBlackHoleCode.
 *
 *  - Lazy blackholing a THUNK:
 *    This is is protected by an explicit write barrier in the thread suspension
 *    code. See ThreadPaused.c:threadPaused.
 *
 *  - Updating a BLACKHOLE:
 *    This case is protected by explicit write barriers in the the update frame
 *    entry code (see rts/Updates.h).
 *
 *  - Blocking on an MVar# (e.g. takeMVar#):
 *    In this case the appropriate MVar primops (e.g. stg_takeMVarzh).  include
 *    explicit memory barriers to ensure that the the newly-allocated
 *    MVAR_TSO_QUEUE is visible to other cores.
 *
 *  - Write to an MVar# (e.g. putMVar#):
 *    This protected by the full barrier implied by the CAS in putMVar#.
 *
 *  - Write to a TVar#:
 *    This is protected by the full barrier implied by the CAS in STM.c:lock_stm.
 *
 *  - Write to an Array#, ArrayArray#, or SmallArray#:
 *    This case is protected by an explicit write barrier in the code produced
 *    for this primop by the codegen. See StgCmmPrim.doWritePtrArrayOp and
 *    StgCmmPrim.doWriteSmallPtrArrayOp. Relevant issue: #12469.
 *
 *  - Write to MutVar# via writeMutVar#:
 *    This case is protected by an explicit write barrier in the code produced
 *    for this primop by the codegen.
 *
 *  - Write to MutVar# via atomicModifyMutVar# or casMutVar#:
 *    This is protected by the full barrier implied by the cmpxchg operations
 *    in this primops.
 *
 *  - Sending a Message to another capability:
 *    This is protected by the acquition and release of the target capability's
 *    lock in Messages.c:sendMessage.
 *
 * Finally, we must ensure that we flush all cores store buffers before
 * entering and leaving GC, since stacks may be read by other cores. This
 * happens as a side-effect of taking and release mutexes (which implies
 * acquire and release barriers, respectively).
 *
 * N.B. recordClosureMutated places a reference to the mutated object on
 * the capability-local mut_list. Consequently this does not require any memory
 * barrier.
 *
 * During parallel GC we need to be careful during evacuation: before replacing
 * a closure with a forwarding pointer we must commit a write barrier to ensure
 * that the copy we made in to-space is visible to other cores.
 *
 * However, we can be a bit lax when *reading* during GC. Specifically, the GC
 * can only make a very limited set of changes to existing closures:
 *
 *  - it can replace a closure's info table with stg_WHITEHOLE.
 *  - it can replace a previously-whitehole'd closure's info table with a
 *    forwarding pointer
 *  - it can replace a previously-whitehole'd closure's info table with a
 *    valid info table pointer (done in eval_thunk_selector)
 *  - it can update the value of a pointer field after evacuating it
 *
 * This is quite nice since we don't need to worry about an interleaving
 * of writes producing an invalid state: a closure's fields remain valid after
 * an update of its info table pointer and vice-versa.
 *
 * After a round of parallel scavenging we must also ensure that any writes the
 * GC thread workers made are visible to the main GC thread. This is ensured by
 * the full barrier implied by the atomic decrement in
 * GC.c:scavenge_until_all_done.
 *
 * The work-stealing queue (WSDeque) also requires barriers; these are
 * documented in WSDeque.c.
 *
 */

/* ----------------------------------------------------------------------------
   Implementations
   ------------------------------------------------------------------------- */

#if !IN_STG_CODE || IN_STGCRUN

/*
 * Exchange the value pointed to by p with w and return the former. This
 * function is used to acquire a lock. An acquire memory barrier is sufficient
 * for a lock operation because corresponding unlock operation issues a
 * store-store barrier (write_barrier()) immediately before releasing the lock.
 */
EXTERN_INLINE StgWord
xchg(StgPtr p, StgWord w)
{
    // When porting GHC to a new platform check that
    // __sync_lock_test_and_set() actually stores w in *p.
    // Use test rts/atomicxchg to verify that the correct value is stored.
    // From the gcc manual:
    // (https://gcc.gnu.org/onlinedocs/gcc-4.4.3/gcc/Atomic-Builtins.html)
    // This built-in function, as described by Intel, is not
    // a traditional test-and-set operation, but rather an atomic
    // exchange operation.
    // [...]
    // Many targets have only minimal support for such locks,
    // and do not support a full exchange operation. In this case,
    // a target may support reduced functionality here by which the
    // only valid value to store is the immediate constant 1. The
    // exact value actually stored in *ptr is implementation defined.
    return __sync_lock_test_and_set(p, w);
}

/*
 * CMPXCHG - the single-word atomic compare-and-exchange instruction.  Used
 * in the STM implementation.
 */
EXTERN_INLINE StgWord
cas(StgVolatilePtr p, StgWord o, StgWord n)
{
    return __sync_val_compare_and_swap(p, o, n);
}

// RRN: Generalized to arbitrary increments to enable fetch-and-add in
// Haskell code (fetchAddIntArray#).
// PT: add-and-fetch, returns new value
EXTERN_INLINE StgWord
atomic_inc(StgVolatilePtr p, StgWord incr)
{
    return __sync_add_and_fetch(p, incr);
}

EXTERN_INLINE StgWord
atomic_dec(StgVolatilePtr p)
{
    return __sync_sub_and_fetch(p, (StgWord) 1);
}

/*
 * Some architectures have a way to tell the CPU that we're in a
 * busy-wait loop, and the processor should look for something else to
 * do (such as run another hardware thread).
 */
EXTERN_INLINE void
busy_wait_nop(void)
{
#if defined(i386_HOST_ARCH) || defined(x86_64_HOST_ARCH)
    // On Intel, the busy-wait-nop instruction is called "pause",
    // which is actually represented as a nop with the rep prefix.
    // On processors before the P4 this behaves as a nop; on P4 and
    // later it might do something clever like yield to another
    // hyperthread.  In any case, Intel recommends putting one
    // of these in a spin lock loop.
    __asm__ __volatile__ ("rep; nop");
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
#if defined(NOSMP)
    return;
#elif defined(i386_HOST_ARCH) || defined(x86_64_HOST_ARCH)
    __asm__ __volatile__ ("" : : : "memory");
#elif defined(powerpc_HOST_ARCH) || defined(powerpc64_HOST_ARCH) \
    || defined(powerpc64le_HOST_ARCH)
    __asm__ __volatile__ ("lwsync" : : : "memory");
#elif defined(sparc_HOST_ARCH)
    /* Sparc in TSO mode does not require store/store barriers. */
    __asm__ __volatile__ ("" : : : "memory");
#elif defined(arm_HOST_ARCH) || defined(aarch64_HOST_ARCH)
    __asm__ __volatile__ ("dmb  st" : : : "memory");
#else
#error memory barriers unimplemented on this architecture
#endif
}

EXTERN_INLINE void
store_load_barrier(void) {
#if defined(NOSMP)
    return;
#elif defined(i386_HOST_ARCH)
    __asm__ __volatile__ ("lock; addl $0,0(%%esp)" : : : "memory");
#elif defined(x86_64_HOST_ARCH)
    __asm__ __volatile__ ("lock; addq $0,0(%%rsp)" : : : "memory");
#elif defined(powerpc_HOST_ARCH) || defined(powerpc64_HOST_ARCH) \
    || defined(powerpc64le_HOST_ARCH)
    __asm__ __volatile__ ("sync" : : : "memory");
#elif defined(sparc_HOST_ARCH)
    __asm__ __volatile__ ("membar #StoreLoad" : : : "memory");
#elif defined(arm_HOST_ARCH)
    __asm__ __volatile__ ("dmb" : : : "memory");
#elif defined(aarch64_HOST_ARCH)
    __asm__ __volatile__ ("dmb sy" : : : "memory");
#else
#error memory barriers unimplemented on this architecture
#endif
}

EXTERN_INLINE void
load_load_barrier(void) {
#if defined(NOSMP)
    return;
#elif defined(i386_HOST_ARCH)
    __asm__ __volatile__ ("" : : : "memory");
#elif defined(x86_64_HOST_ARCH)
    __asm__ __volatile__ ("" : : : "memory");
#elif defined(powerpc_HOST_ARCH) || defined(powerpc64_HOST_ARCH) \
    || defined(powerpc64le_HOST_ARCH)
    __asm__ __volatile__ ("lwsync" : : : "memory");
#elif defined(sparc_HOST_ARCH)
    /* Sparc in TSO mode does not require load/load barriers. */
    __asm__ __volatile__ ("" : : : "memory");
#elif defined(arm_HOST_ARCH)
    __asm__ __volatile__ ("dmb" : : : "memory");
#elif defined(aarch64_HOST_ARCH)
    __asm__ __volatile__ ("dmb sy" : : : "memory");
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
