/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2005-2011
 *
 * Macros for multi-CPU support
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * -------------------------------------------------------------------------- */

#pragma once

#if defined(arm_HOST_ARCH) && defined(arm_HOST_ARCH_PRE_ARMv6)
void arm_atomic_spin_lock(void);
void arm_atomic_spin_unlock(void);
#endif

// Unconditionally atomic operations
// These are atomic even in the non-threaded RTS. These are necessary in the
// Proftimer implementation, which may be called from the pthreads-based
// ITimer implementation.
#define RELAXED_LOAD_ALWAYS(ptr) __atomic_load_n(ptr, __ATOMIC_RELAXED)
#define RELAXED_STORE_ALWAYS(ptr,val) __atomic_store_n(ptr, val, __ATOMIC_RELAXED)
#define RELAXED_ADD_ALWAYS(ptr,val) __atomic_add_fetch(ptr, val, __ATOMIC_RELAXED)

// Acquire/release atomic operations
#define ACQUIRE_LOAD_ALWAYS(ptr) __atomic_load_n(ptr, __ATOMIC_ACQUIRE)
#define RELEASE_STORE_ALWAYS(ptr,val) __atomic_store_n(ptr, val, __ATOMIC_RELEASE)

// Sequentially consistent atomic operations
#define SEQ_CST_LOAD_ALWAYS(ptr) __atomic_load_n(ptr, __ATOMIC_SEQ_CST)
#define SEQ_CST_STORE_ALWAYS(ptr,val) __atomic_store_n(ptr, val, __ATOMIC_SEQ_CST)
#define SEQ_CST_ADD_ALWAYS(ptr,val) __atomic_add_fetch(ptr, val, __ATOMIC_SEQ_CST)
#define SEQ_CST_SUB_ALWAYS(ptr,val) __atomic_sub_fetch(ptr, val, __ATOMIC_SEQ_CST)
#define SEQ_CST_XCHG_ALWAYS(ptr,val) __atomic_exchange_n(ptr, val, __ATOMIC_SEQ_CST);

#if defined(THREADED_RTS)

/* ----------------------------------------------------------------------------
   Atomic operations
   ------------------------------------------------------------------------- */

#if !IN_STG_CODE || IN_STGCRUN
/*
 * The atomic exchange operation: xchg(p,w) exchanges the value
 * pointed to by p with the value w, returning the old value.
 *
 * Used for locking closures during updates (see lockClosure()
 * in rts/include/rts/storage/SMPClosureOps.h) and the MVar primops.
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
EXTERN_INLINE StgWord8 cas_word8(StgWord8 *volatile p, StgWord8 o, StgWord8 n);

/*
 * Compare-and-swap
 * this uses a seq_cst success memory order and a relaxed failure memory order
 */
EXTERN_INLINE StgWord cas_seq_cst_relaxed(StgVolatilePtr p, StgWord o, StgWord n);


/*
 * Atomic addition by the provided quantity
 *
 * atomic_inc(p, n) {
 *   return ((*p) += n);
 * }
 */
EXTERN_INLINE StgWord atomic_inc(StgVolatilePtr p, StgWord n);

/*
 * Atomic subtraction by the provided quantity.
 *
 * atomic_dec(p, n) {
 *   return ((*p) -= n);
 * }
 */
EXTERN_INLINE StgWord atomic_dec(StgVolatilePtr p, StgWord n);

/*
 * Busy-wait nop: this is a hint to the CPU that we are currently in a
 * busy-wait loop waiting for another CPU to change something.  On a
 * hyperthreaded CPU it should yield to another thread, for example.
 */
EXTERN_INLINE void busy_wait_nop(void);

#endif // !IN_STG_CODE

/*
 * Note [C11 memory model]
 * ~~~~~~~~~~~~~~~~~~~~~~~
 * When it comes to memory, real multiprocessors provide a wide range of
 * concurrency semantics due to out-of-order execution and caching.
 * To provide consistent reasoning across architectures, GHC relies the C11
 * memory model. Not only does this provide a well-studied, fairly
 * easy-to-understand conceptual model, but the C11 memory model gives us
 * access to a number of tools which help us verify the compiler (see Note
 * [ThreadSanitizer] in rts/include/rts/TSANUtils.h).
 *
 * Under the C11 model, each processor can be imagined to have a potentially
 * out-of-date view onto the system's memory, which can be manipulated with two
 * classes of memory operations:
 *
 *  - non-atomic operations (e.g. loads and stores) operate strictly on the
 *    processor's local view of memory and consequently may not be visible
 *    from other processors.
 *
 *  - atomic operations (e.g. load, store, fetch-and-{add,subtract,and,or},
 *    exchange, and compare-and-swap) parametrized by ordering semantics.
 *
 * The ordering semantics of an operation (acquire, release, or sequentially
 * consistent) will determine the amount of synchronization the operation
 * requires.
 *
 * A processor may synchronize its
 * view of memory with that of another processor by performing an atomic
 * memory operation.
 *
 * While non-atomic operations can be thought of as operating on a local
 *
 * See also:
 *
 *   - The C11 standard, ISO/IEC 14882 2011.
 *
 *   - Boehm, Adve. "Foundations of the C++ Concurrency Memory Model."
 *     PLDI '08.
 *
 *   - Batty, Owens, Sarkar, Sewall, Weber. "Mathematizing C++ Concurrency."
 *     POPL '11.
 *
 * Note [Heap memory barriers]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Machines with weak memory ordering semantics have consequences for how
 * closures are observed and mutated. In particular, when we
 * expose a new (or mutated) object to another core we must ensure that the
 * stores which formed the new object are visible (e.g. stores are flushed from
 * cache and the relevant cachelines invalidated in other cores).
 *
 * To ensure this we must issue memory barriers when accessing closures and
 * their fields. Since reasoning about concurrent memory access with barriers tends to be
 * subtle and platform dependent, it is more common to instead write programs
 * in terms of an abstract memory model and let the compiler (GHC and the
 * system's C compiler) worry about what barriers are needed to realize the
 * requested semantics on the target system. GHC relies on the widely used C11
 * memory model for this; see Note [C11 memory model] for a brief introduction.
 *
 * Also note that the majority of this Note are only concerned with mutation
 * by the mutator. The GC is free to change nearly any field (which is
 * necessary for a moving GC). Naturally, doing this safely requires care which
 * we discuss in the "Barriers during GC" section below.
 *
 * Field access
 * ------------
 * Which barriers are required to access a field of a closure depends upon the
 * identity of the field. In general, fields come in three flavours:
 *
 *  * Mutable GC Pointers (C type `StgClosure*`, Cmm type `StgPtr`)
 *  * Immutable GC Pointers (C type `MUT_FIELD StgClosure*`, Cmm type `StgPtr`)
 *  * Non-pointers (C type `StgWord`, Cmm type `StgWord`)
 *
 * Note that Addr# fields are *not* GC pointers and therefore are classified
 * as non-pointers. In this case responsibility for barriers lies with the
 * party dereferencing the Addr#.
 *
 * Immutable pointer fields are those which the mutator cannot change after
 * an object is made visible on the heap. Most objects' fields are of this
 * flavour (e.g. all data constructor fields). As these fields are written
 * precisely once, no write barriers are needed on writes nor reads. This is
 * safe due to an argument hinging on causality: Consider an immutable field F
 * of an object O which refers to object O'. Naturally, O' must have been
 * visible to the creator of O when O was constructed. Consequently, if O is
 * visible to a reader, O' must also be visible to the same reader.
 *
 * Mutable pointer fields are those which can be modified by the mutator. These
 * require a bit more care as they may break the causality argument given
 * above. For instance, consider an object O (e.g. a MUT_VAR) with a mutable
 * field F. A thread may allocate a new object O' and store a reference to O'
 * into F. Without explicit synchronization O' may not be visible to another
 * thread attempting to dereference F.
 *
 * To ensure the visibility of the referent, writing to a mutable pointer field
 * must be done via a release-store. Conversely, reading from such a field is
 * done via an acquire-load.
 *
 * Mutable fields include:
 *
 *   - StgMutVar: var
 *   - StgWeak: finalizer
 *   - StgMVar: head, tail, value
 *   - StgMVarTSOQueue: link
 *   - StgTVar: current_value, first_watch_queue_entry
 *   - StgTVarWatchQueue: {next,prev}_queue_entry
 *   - StgTRecChunk: TODO
 *   - StgMutArrPtrs: payload
 *   - StgSmallMutArrPtrs: payload
 *   - StgThunk although this is a somewhat special case; see below
 *   - StgInd: indirectee
 *   - StgTSO: block_info
 *
 * Finally, non-pointer fields can be safely mutated without barriers as
 * they do not refer to other memory locations. Technically, concurrent
 * accesses to non-pointer fields still do need to be atomic in many cases to
 * avoid torn accesses. However, this is something that we generally avoid by
 * locking closures prior to mutating non-pointer fields (see Locking closures
 * below).
 *
 * Locking closures
 * ----------------
 * Several primops temporarily turn closures into WHITEHOLEs to ensure that
 * they have exclusive access (see SMPClosureOps.h:reallyLockClosure).
 * These include,
 *
 *   - takeMVar#, tryTakeMVar#
 *   - putMVar#, tryPutMVar#
 *   - readMVar#, tryReadMVar#
 *   - readIOPort#
 *   - writeIOPort#
 *   - addCFinalizerToWeak#
 *   - finalizeWeak#
 *   - deRefWeak#
 *
 * Locking is done via an atomic exchange operation on the closure's info table
 * pointer with sequential consistency (although only acquire ordering is
 * needed). Similarly, unlocking is also done with an atomic exchange to
 * restore the closure's original info table pointer (although
 * this time only the release ordering is needed). This ensures
 * that we synchronize with any previous thread that had locked the closure.
 *
 * Thunks
 * ------
 * As noted above, thunks are a rather special (yet quite common) case. In
 * particular, they have the unique property of being updatable (that is, can
 * be transformed from a thunk into an indirection after evaluation). This
 * transformation requires its own synchronization protocol to mediate the
 * interaction between the updater and the reader. In particular, we
 * must ensure that a reader examining a thunk being updated by another core
 * can see the indirectee. Consequently, a thunk update (see rts/Updates.h)
 * does the following:
 *
 *  U1. use a release-store to place the new indirectee into the thunk's
 *      indirectee field
 *
 *  U2. use a release-store to set the info table to stg_BLACKHOLE (which
 *      represents an indirection)
 *
 * Blackholing a thunk (either eagerly, by GHC.StgToCmm.Bind.emitBlackHoleCode,
 * or lazily, by ThreadPaused.c:threadPaused) is done similarly.
 *
 * Conversely, entering an indirection (see the entry code of stg_BLACKHOLE,
 * stg_IND, and stg_IND_STATIC in rts/StgMiscClosure.cmm) does the
 * following:
 *
 *  E1. jump into the entry code of the indirection (e.g. stg_BLACKHOLE);
 *      this of course implies that we have already read the thunk's info table
 *      pointer, which is done with a relaxed load.
 *
 *  E2. acquire-fence
 *
 *  E3. acquire-load the indirectee. Since thunks are updated at most
 *      once we know that the fence in the last step has given us
 *      an up-to-date view of the indirectee closure.
 *
 *  E4. enter the indirectee (or block if the indirectee is a TSO)
 *
 * The release/acquire pair (U2)/(E2) is somewhat surprising but is necessary as
 * the C11 memory model does not guarantee that the store (U1) is visible to
 * (E3) despite (U1) preceding (U2) in program-order (due to the relaxed
 * ordering of (E3)). This is demonstrated by the following CppMem model:
 *
 *     int main() {
 *       atomic_int x = 0;    // info table pointer
 *       atomic_int y = 0;    // indirectee
 *       {{{
 *         {    // blackhole update
 *           y.store(1, memory_order_release);                // U1
 *           x.store(2, memory_order_release);                // U2
 *         }
 *       |||
 *         {    // blackhole entry
 *           r1=x.load(memory_order_relaxed).readsvalue(2);   // E1
 *           //fence(memory_order_acquire);                   // E2
 *           r2=y.load(memory_order_acquire);                 // E3
 *         }
 *       }}};
 *       return 0;
 *     }
 *
 * Under the C11 memory model this program admits an execution where the
 * indirectee `r2=0`.
 *
 * Of course, this could also be addressed by strengthing the ordering of (E1)
 * to acquire, but this would incur a significant cost on every closure entry
 * (including non-blackholes).
 *
 * Other closures
 * --------------
 * Below is a summary of the barriers which are necessary to make GHC's
 * primitive types respect the above rules:
 *
 *  - MVar# operations.
 *    The MVar# primops lock the MVar prior to reading or writing any MVar fix.
 *
 *  - Write to a TVar#:
 *    This is protected by the full barrier implied by the CAS in STM.c:lock_stm.
 *
 *  - Write to an Array# or SmallArray#:
 *    This case is protected by an explicit write barrier in the code produced
 *    for this primop by the codegen. See GHC.StgToCmm.Prim.doWritePtrArrayOp and
 *    GHC.StgToCmm.Prim.doWriteSmallPtrArrayOp. Relevant issue: #12469.
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
 * N.B. recordClosureMutated places a reference to the mutated object on
 * the capability-local mut_list. Consequently this does not require any memory
 * barrier.
 *
 * Barriers in thread blocking
 * ---------------------------
 * When a thread blocks (e.g. on an MVar) it will typically allocate a heap object
 * to record its blocked-ness (e.g. a StgMVarTSOQueue), expose this via
 * StgTSO.block_info, and update StgTSO.why_blocked to record the reason for
 * its blocking. The visibility of the block_info is guaranteed by the ordering
 * of the why_blocked update.
 *
 * Barriers in thread migration
 * ----------------------------
 * When a thread is migrated from one capability to another we must take care
 * that the new capability synchronizes with the old (e.g. to ensure that
 * the state of the thread's stack is visible to the new executor).
 * This is ensured by the barriers implied in sending the MessageWakeup.
 *
 * Barriers on Messages
 * --------------------
 * The RTS uses the Message mechanism to convey information between capabilities.
 * To send a message (see Messages.c:sendMessage) the sender must first take
 * the `lock` of the recipient capability. The Message can then be linked
 * onto the recipient's `inbox` list and release the lock (implying a release
 * barrier).
 *
 * To process its inbox (see Schedule.c:scheduleProcessInbox) the recipient must
 * take the capability lock before examining the inbox. This implies an acquire
 * barrier, ensuring that the messages in the inbox are visible.
 *
 * Under the above story there is no need for accesses to `inbox` to be
 * atomic at all as all accesses are under the capability lock. However, there
 * is a slight wrinkle here:
 * Capability.h:emptyInbox wants to test whether `inbox` is empty without
 * holding the capability lock. Consequently, accesses to `inbox` must be
 * atomic, although may use the relaxed ordering.
 *
 * Barriers during GC
 * ------------------
 * Entering the garbage collector implies an acquire barrier across all
 * capabilities as all capabilities obey the following protocol when entering
 * `gcWorkerThread`:
 *
 *  1. acquire gc_entry_mutex
 *  2. signal gc_entry_arrived_cv to let the GC leader know
 *     that we have started to synchronize
 *  3. wait on gc_entry_start_now_cv, releasing
 *     gc_entry_mutex; this implies a release barrier
 *  4. after all threads have synchronized, the leader will
 *     broadcast signal gc_entry_start_now_cv
 *  5. each of the waiting workers will acquire
 *     gc_entry_mutex, implying an acquire barrier which will
 *     synchronize with all of the other workers' releases in
 *     (3).
 *
 * Similarly, leaving the garbage collector implies a release
 * barrier as gcWorkerThread blocks on gc_exit_leave_now_cv
 * with a similar protocol.
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
 * Verifying memory ordering
 * -------------------------
 * To verify that GHC's RTS and the code produced by the compiler are free of
 * data races we employ ThreadSaniziter. See Note [ThreadSanitizer] in TSANUtils.h
 * for details on this facility.
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
 * store-store barrier (release-store) immediately before releasing the lock.
 */
EXTERN_INLINE StgWord
xchg(StgPtr p, StgWord w)
{
    return __atomic_exchange_n(p, w, __ATOMIC_SEQ_CST);
}

/*
 * CMPXCHG - the single-word atomic compare-and-exchange instruction.  Used
 * in the STM implementation.
 */
EXTERN_INLINE StgWord
cas(StgVolatilePtr p, StgWord o, StgWord n)
{
    __atomic_compare_exchange_n(p, &o, n, 0, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
    return o;
}

EXTERN_INLINE StgWord8
cas_word8(StgWord8 *volatile p, StgWord8 o, StgWord8 n)
{
    __atomic_compare_exchange_n(p, &o, n, 0, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
    return o;
}

EXTERN_INLINE StgWord
cas_seq_cst_relaxed(StgVolatilePtr p, StgWord o, StgWord n) {
    __atomic_compare_exchange_n(p, &o, n, 0, __ATOMIC_SEQ_CST, __ATOMIC_RELAXED);
    return o;
}

// RRN: Generalized to arbitrary increments to enable fetch-and-add in
// Haskell code (fetchAddIntArray#).
// PT: add-and-fetch, returns new value
EXTERN_INLINE StgWord
atomic_inc(StgVolatilePtr p, StgWord incr)
{
    return __atomic_add_fetch(p, incr, __ATOMIC_SEQ_CST);
}

EXTERN_INLINE StgWord
atomic_dec(StgVolatilePtr p, StgWord decr)
{
    return __atomic_sub_fetch(p, decr, __ATOMIC_SEQ_CST);
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

// Load a pointer from a memory location that might be being modified
// concurrently.  This prevents the compiler from optimising away
// multiple loads of the memory location, as it might otherwise do in
// a busy wait loop for example.
#define VOLATILE_LOAD(p) (*((StgVolatilePtr)(p)))

// Relaxed atomic operations.
#define RELAXED_LOAD(ptr) __atomic_load_n(ptr, __ATOMIC_RELAXED)
#define RELAXED_STORE(ptr,val) __atomic_store_n(ptr, val, __ATOMIC_RELAXED)
#define RELAXED_ADD(ptr,val) __atomic_add_fetch(ptr, val, __ATOMIC_RELAXED)

// Acquire/release atomic operations
#define ACQUIRE_LOAD(ptr) __atomic_load_n(ptr, __ATOMIC_ACQUIRE)
#define RELEASE_STORE(ptr,val) __atomic_store_n(ptr, val, __ATOMIC_RELEASE)

// Sequentially consistent atomic operations
#define SEQ_CST_LOAD(ptr) __atomic_load_n(ptr, __ATOMIC_SEQ_CST)
#define SEQ_CST_STORE(ptr,val) __atomic_store_n(ptr, val, __ATOMIC_SEQ_CST)
#define SEQ_CST_ADD(ptr,val) __atomic_add_fetch(ptr, val, __ATOMIC_SEQ_CST)

// Non-atomic addition for "approximate" counters that can be lossy
#define NONATOMIC_ADD(ptr,val) RELAXED_STORE(ptr, RELAXED_LOAD(ptr) + val)

// compare-and-swap atomic operations
#define SEQ_CST_RELAXED_CAS(p,o,n) cas_seq_cst_relaxed(p,o,n)

// Explicit fences
//
// These are typically necessary only in very specific cases (e.g. WSDeque)
// where the ordered operations aren't expressive enough to capture the desired
// ordering.
//
// Additionally, it is preferable to use the *_FENCE_ON() forms, which turn into
// memory accesses when compiling for ThreadSanitizer (as ThreadSanitizer is
// otherwise unable to reason about fences). See Note [ThreadSanitizer] in
// TSANUtils.h.

#define ACQUIRE_FENCE() __atomic_thread_fence(__ATOMIC_ACQUIRE)
#define RELEASE_FENCE() __atomic_thread_fence(__ATOMIC_RELEASE)
#define SEQ_CST_FENCE() __atomic_thread_fence(__ATOMIC_SEQ_CST)

#if defined(TSAN_ENABLED)
#if !defined(__clang__)
#undef ACQUIRE_FENCE
#undef RELEASE_FENCE
#undef SEQ_CST_FENCE
#define ACQUIRE_FENCE() NO_WARN(-Wtsan, __atomic_thread_fence(__ATOMIC_ACQUIRE);)
#define RELEASE_FENCE() NO_WARN(-Wtsan, __atomic_thread_fence(__ATOMIC_RELEASE);)
#define SEQ_CST_FENCE() NO_WARN(-Wtsan, __atomic_thread_fence(__ATOMIC_SEQ_CST);)
#endif
#define ACQUIRE_FENCE_ON(x) (void)ACQUIRE_LOAD(x)
#else
#define ACQUIRE_FENCE_ON(x) __atomic_thread_fence(__ATOMIC_ACQUIRE)
#endif

/* ---------------------------------------------------------------------- */
#else /* !THREADED_RTS */

// Relaxed atomic operations
#define RELAXED_LOAD(ptr) *ptr
#define RELAXED_STORE(ptr,val) *ptr = val
#define RELAXED_ADD(ptr,val) *ptr += val

// Acquire/release atomic operations
#define ACQUIRE_LOAD(ptr) *ptr
#define RELEASE_STORE(ptr,val) *ptr = val

// Sequentially consistent atomic operations
#define SEQ_CST_LOAD(ptr) *ptr
#define SEQ_CST_STORE(ptr,val) *ptr = val
#define SEQ_CST_ADD(ptr,val) *ptr += val

// Non-atomic addition for "approximate" counters that can be lossy
#define NONATOMIC_ADD(ptr,val) *ptr += val

// compare-and-swap atomic operations
#define SEQ_CST_RELAXED_CAS(p,o,n) cas(p,o,n)

// Fences
#define ACQUIRE_FENCE()
#define RELEASE_FENCE()
#define SEQ_CST_FENCE()
#define ACQUIRE_FENCE_ON(x)
#define RELEASE_FENCE_ON(x)

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

EXTERN_INLINE StgWord8 cas_word8(StgWord8 *volatile p, StgWord8 o, StgWord8 n);
EXTERN_INLINE StgWord8
cas_word8(StgWord8 *volatile p, StgWord8 o, StgWord8 n)
{
    StgWord8 result;
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
atomic_dec(StgVolatilePtr p, StgWord decr)
{
    return ((*p) -= decr);
}
#endif

/* An alias for the C11 declspec */
#define ATOMIC

#define VOLATILE_LOAD(p) ((StgWord)*((StgWord*)(p)))

#endif /* !THREADED_RTS */

/* Helpers implemented in terms of the above */
#if !IN_STG_CODE || IN_STGCRUN

INLINE_HEADER void *
xchg_ptr(void **p, void *w)
{
    return (void *) xchg((StgPtr) p, (StgWord) w);
}

INLINE_HEADER void *
cas_ptr(volatile void **p, void *o, void *n)
{
    return (void *) cas((StgVolatilePtr) p, (StgWord) o, (StgWord) n);
}

#endif
