/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * External Storage Manger Interface
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include <stddef.h>
#include "rts/OSThreads.h"

/* -----------------------------------------------------------------------------
 * Generational GC
 *
 * We support an arbitrary number of generations.  Notes (in no particular
 * order):
 *
 *       - Objects "age" in the nursery for one GC cycle before being promoted
 *         to the next generation.  There is no aging in other generations.
 *
 *       - generation 0 is the allocation area.  It is given
 *         a fixed set of blocks during initialisation, and these blocks
 *         normally stay in G0S0.  In parallel execution, each
 *         Capability has its own nursery.
 *
 *       - during garbage collection, each generation which is an
 *         evacuation destination (i.e. all generations except G0) is
 *         allocated a to-space.  evacuated objects are allocated into
 *         the generation's to-space until GC is finished, when the
 *         original generations's contents may be freed and replaced
 *         by the to-space.
 *
 *       - the mutable-list is per-generation.  G0 doesn't have one
 *         (since every garbage collection collects at least G0).
 *
 *       - block descriptors contain a pointer to the generation that
 *         the block belongs to, for convenience.
 *
 *       - static objects are stored in per-generation lists.  See GC.c for
 *         details of how we collect CAFs in the generational scheme.
 *
 *       - large objects are per-generation, and are promoted in the
 *         same way as small objects.
 *
 * ------------------------------------------------------------------------- */

// A count of blocks needs to store anything up to the size of memory
// divided by the block size.  The safest thing is therefore to use a
// type that can store the full range of memory addresses,
// ie. StgWord.  Note that we have had some tricky int overflows in a
// couple of cases caused by using ints rather than longs (e.g. #5086)

typedef StgWord memcount;

typedef struct nursery_ {
    bdescr *       blocks;
    memcount       n_blocks;
} nursery;

// Nursery invariants:
//
//  - cap->r.rNursery points to the nursery for this capability
//
//  - cap->r.rCurrentNursery points to the block in the nursery that we are
//    currently allocating into.  While in Haskell the current heap pointer is
//    in Hp, outside Haskell it is stored in cap->r.rCurrentNursery->free.
//
//  - the blocks *after* cap->rCurrentNursery in the chain are empty
//    (although their bd->free pointers have not been updated to
//    reflect that)
//
//  - the blocks *before* cap->rCurrentNursery have been used.  Except
//    for rCurrentAlloc.
//
//  - cap->r.rCurrentAlloc is either NULL, or it points to a block in
//    the nursery *before* cap->r.rCurrentNursery.
//
// See also Note [allocation accounting] to understand how total
// memory allocation is tracked.

typedef struct generation_ {
    uint32_t       no;                  // generation number

    bdescr *       blocks;              // blocks in this gen
    memcount       n_blocks;            // number of blocks
    memcount       n_words;             // number of used words

    bdescr *       large_objects;       // large objects (doubly linked)
    memcount       n_large_blocks;      // no. of blocks used by large objs
    memcount       n_large_words;       // no. of words used by large objs
    memcount       n_new_large_words;   // words of new large objects
                                        // (for doYouWantToGC())

    bdescr *       compact_objects;     // compact objects chain
                                        // the second block in each compact is
                                        // linked from the closure object, while
                                        // the second compact object in the
                                        // chain is linked from bd->link (like
                                        // large objects)
    memcount       n_compact_blocks;    // no. of blocks used by all compacts
    bdescr *       compact_blocks_in_import; // compact objects being imported
                                             // (not known to the GC because
                                             // potentially invalid, but we
                                             // need to keep track of them
                                             // to avoid assertions in Sanity)
                                             // this is a list shaped like compact_objects
    memcount       n_compact_blocks_in_import; // no. of blocks used by compacts
                                               // being imported

    // Max blocks to allocate in this generation before collecting it. Collect
    // this generation when
    //
    //     n_blocks + n_large_blocks + n_compact_blocks > max_blocks
    //
    memcount       max_blocks;

    StgTSO *       threads;             // threads in this generation.
                                        // linked via global_link.
                                        // protected by sched_mutex except during GC.
    StgWeak *      weak_ptr_list;       // weak pointers in this gen

    struct generation_ *to;             // destination gen for live objects

    // stats information
    uint32_t collections;
    uint32_t par_collections;
    uint32_t failed_promotions;         // Currently unused

    // ------------------------------------
    // Fields below are used during GC only

#if defined(THREADED_RTS)
    char pad[128];                      // make sure the following is
                                        // on a separate cache line.
    SpinLock     sync;                  // lock for large_objects
                                        //    and scavenged_large_objects
#endif

    int          mark;                  // mark (not copy)? (old gen only)
    int          compact;               // compact (not sweep)? (old gen only)

    // During GC, if we are collecting this gen, blocks and n_blocks
    // are copied into the following two fields.  After GC, these blocks
    // are freed.
    bdescr *     old_blocks;            // bdescr of first from-space block
    memcount     n_old_blocks;         // number of blocks in from-space
    memcount     live_estimate;         // for sweeping: estimate of live data

    bdescr *     scavenged_large_objects;  // live large objs after GC (d-link)
    memcount     n_scavenged_large_blocks; // size (not count) of above

    bdescr *     live_compact_objects;  // live compact objs after GC (d-link)
    memcount     n_live_compact_blocks; // size (not count) of above

    bdescr *     bitmap;                // bitmap for compacting collection

    StgTSO *     old_threads;
    StgWeak *    old_weak_ptr_list;
} generation;

extern generation * generations;
extern generation * g0;
extern generation * oldest_gen;

typedef void(*ListBlocksCb)(void *user, bdescr *);
void listAllBlocks(ListBlocksCb cb, void *user);

/* -----------------------------------------------------------------------------
   Generic allocation

   See: Note [allocate and allocateMightFail]
        Note [allocatePinned]
        Note [allocate failure]
   -------------------------------------------------------------------------- */

StgPtr  allocate          ( Capability *cap, W_ n );
StgPtr  allocateMightFail ( Capability *cap, W_ n );
StgPtr  allocatePinned    ( Capability *cap, W_ n, W_ alignment, W_ align_off);

/* Note [allocate and allocateMightFail]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   allocate() and allocateMightFail() allocate an area of memory n
   *words* large, from the nursery of the supplied Capability, or from
   the global block pool if the area requested is larger than
   LARGE_OBJECT_THRESHOLD.  Memory is not allocated from the current
   nursery block, so as not to interfere with Hp/HpLim.

   The address of the allocated memory is returned.

   After allocating, fill in the heap closure header, e.g.
   SET_HDR(arr, stg_MUT_ARR_PTRS_DIRTY_info, CCCS);
   and call TICK_ALLOC_XXX(...) for the benefit of the ticky-ticky
   profiler.

   On allocation failure, allocateMightFail() returns NULL whereas
   allocate() terminates the RTS. See Note [allocate failure]. You
   should prefer allocateMightFail() in cases where you can propagate
   the failure up to a context in which you can raise exceptions, e.g.
   in primops.
 */

/* Note [allocatePinned]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   allocatePinned() allocates a chunk of contiguous store n *words*
   long, which is at a fixed address (i.e. won't be moved by GC). The
   word at the byte offset 'align_off' will be aligned to 'alignment',
   which must be a power of two.

   The address of the allocated memory is returned.

   The GC can't in general handle pinned objects, so allocatePinned()
   can only be used for ByteArrays / stg_ARR_WORDS at the moment.

   On allocation failure, allocatePinned() returns NULL.
   See Note [allocate failure].
 */

/* Note [allocate failure]
   ~~~~~~~~~~~~~~~~~~~~~~~

   The allocation functions differ in how they handle failure to
   allocate:

    * on failure allocateMightFail() returns NULL
    * on failure allocatePinned() returns NULL
    * on failure allocate() terminates the RTS (and thus typically
      the whole process)

   Each of these functions tries _quite_ hard to avoid allocation
   failure however. If the nursery is already full, then another block
   is allocated from the global block pool. If we need to get memory
   from the OS and that operation fails, or if we would exceed
   maxHeapSize then we fail.

   There are two main existing conventions within the RTS for handling
   allocation failure.

   1. Start from a primop that uses one of the MAYBE_GC_* macros to
      provide an opportunity to GC. Then buried deeply within C code
      called from the primop, use allocate().

   2. Start from a primop that uses one of the MAYBE_GC_* macros to
      provide an opportunity to GC. Use allocateMightFail() within the
      C code called from the primop. If that fails, propagate the
      failure up to the primop where it can throw a HeapOverflow
      exception.

   Being able to throw an exception is preferable, since it's more
   polite, provides better reporting and potentially it can be
   caught and handled by the user program.

   An advantage of the first approach is that its simpler to implement.
   It does not require any mechanism to propagate failure (and undoing
   any effects along the way so the operation can be safely retried
   after GC).

   Arguably neither existing convention is ideal. One might imagine
   that when failure from allocateMightFail() propagates to the top
   level primop, the primop would not throw a HeapOverflow exception
   but invoke the GC with a request to make available at least the
   required number of words. The GC may be able to succeed, in which
   case the original operation can be retried. Or if the GC is unable
   to free enough memory then it can throw the HeapOverflow exception.
   In practice however, though there is a mechanism (via HpAlloc) to
   tell the GC how much memory was needed, this is not used to decide
   if we have to fail the allocation, it is just used for error
   reporting.
 */

/* memory allocator for executable memory */
typedef void* AdjustorWritable;
typedef void* AdjustorExecutable;

void flushExec(W_ len, AdjustorExecutable exec_addr);

// Used by GC checks in external .cmm code:
extern W_ large_alloc_lim;

// Should triggering an allocation limit kill the thread
// and should we run a user-defined hook when it is triggered.
void setAllocLimitKill(bool, bool);

/* -----------------------------------------------------------------------------
   Performing Garbage Collection
   -------------------------------------------------------------------------- */

void performGC(void);
void performMajorGC(void);
void performBlockingMajorGC(void);

/* -----------------------------------------------------------------------------
   The CAF table - used to let us revert CAFs in GHCi
   -------------------------------------------------------------------------- */

StgInd *newCAF         (StgRegTable *reg, StgIndStatic *caf);
StgInd *newRetainedCAF (StgRegTable *reg, StgIndStatic *caf);
StgInd *newGCdCAF      (StgRegTable *reg, StgIndStatic *caf);
void revertCAFs (void);

// Request that all CAFs are retained indefinitely.
// (preferably use RtsConfig.keep_cafs instead)
void setKeepCAFs (void);

// Let the runtime know that all the CAFs in high mem are not
// to be retained. Useful in conjunction with loadNativeObj
void setHighMemDynamic (void);

/* -----------------------------------------------------------------------------
   This is the write barrier for MUT_VARs, a.k.a. IORefs.  A
   MUT_VAR_CLEAN object is not on the mutable list; a MUT_VAR_DIRTY
   is.  When written to, a MUT_VAR_CLEAN turns into a MUT_VAR_DIRTY
   and is put on the mutable list.
   -------------------------------------------------------------------------- */

void dirty_MUT_VAR(StgRegTable *reg, StgMutVar *mv, StgClosure *old);

/* set to disable CAF garbage collection in GHCi. */
/* (needed when dynamic libraries are used). */
extern bool keepCAFs;

#include "rts/Flags.h"

INLINE_HEADER void initBdescr(bdescr *bd, generation *gen, generation *dest)
{
    RELAXED_STORE(&bd->gen, gen);
    RELAXED_STORE(&bd->gen_no, gen->no);
    RELAXED_STORE(&bd->dest_no, dest->no);

#if !IN_STG_CODE
    /* See Note [RtsFlags is a pointer in STG code] */
    ASSERT(gen->no < RtsFlags.GcFlags.generations);
    ASSERT(dest->no < RtsFlags.GcFlags.generations);
#endif
}
