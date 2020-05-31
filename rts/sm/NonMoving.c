/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2018
 *
 * Non-moving garbage collector and allocator
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "RtsUtils.h"
#include "Capability.h"
#include "Sparks.h"
#include "Printer.h"
#include "Storage.h"
// We call evacuate, which expects the thread-local gc_thread to be valid;
// This is sometimes declared as a register variable therefore it is necessary
// to include the declaration so that the compiler doesn't clobber the register.
#include "GCThread.h"
#include "GCTDecl.h"
#include "Schedule.h"
#include "Stats.h"

#include "NonMoving.h"
#include "NonMovingMark.h"
#include "NonMovingSweep.h"
#include "NonMovingCensus.h"
#include "StablePtr.h" // markStablePtrTable
#include "Schedule.h" // markScheduler
#include "Weak.h" // dead_weak_ptr_list

struct NonmovingHeap nonmovingHeap;

uint8_t nonmovingMarkEpoch = 1;

static void nonmovingBumpEpoch(void) {
    nonmovingMarkEpoch = nonmovingMarkEpoch == 1 ? 2 : 1;
}

#if defined(THREADED_RTS)
/*
 * This mutex ensures that only one non-moving collection is active at a time.
 */
Mutex nonmoving_collection_mutex;

OSThreadId mark_thread;
bool concurrent_coll_running = false;
Condition concurrent_coll_finished;
Mutex concurrent_coll_finished_lock;
#endif

/*
 * Note [Non-moving garbage collector]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * The sources rts/NonMoving*.c implement GHC's non-moving garbage collector
 * for the oldest generation. In contrast to the throughput-oriented moving
 * collector, the non-moving collector is designed to achieve low GC latencies
 * on large heaps. It accomplishes low-latencies by way of a concurrent
 * mark-and-sweep collection strategy on a specially-designed heap structure.
 * While the design is described in detail in the design document found in
 * docs/storage/nonmoving-gc, we briefly summarize the structure here.
 *
 *
 * === Heap Structure ===
 *
 * The nonmoving heap (embodied by struct NonmovingHeap) consists of a family
 * of allocators, each serving a range of allocation sizes. Each allocator
 * consists of a set of *segments*, each of which contain fixed-size *blocks*
 * (not to be confused with "blocks" provided by GHC's block allocator; this is
 * admittedly an unfortunate overlap in terminology).  These blocks are the
 * backing store for the allocator. In addition to blocks, the segment also
 * contains some header information (see struct NonmovingSegment in
 * NonMoving.h). This header contains a *bitmap* encoding one byte per block
 * (used by the collector to record liveness), as well as the index of the next
 * unallocated block (and a *snapshot* of this field which will be described in
 * the next section).
 *
 * Each allocator maintains three sets of segments:
 *
 *  - A *current* segment for each capability; this is the segment which that
 *    capability will allocate into.
 *
 *  - A pool of *active* segments, each of which containing at least one
 *    unallocated block. The allocate will take a segment from this pool when
 *    it fills its *current* segment.
 *
 *  - A set of *filled* segments, which contain no unallocated blocks and will
 *    be collected during the next major GC cycle
 *
 * Storage for segments is allocated using the block allocator using an aligned
 * group of NONMOVING_SEGMENT_BLOCKS blocks. This makes the task of locating
 * the segment header for a clone a simple matter of bit-masking (as
 * implemented by nonmovingGetSegment).
 *
 * In addition, to relieve pressure on the block allocator we keep a small pool
 * of free blocks around (nonmovingHeap.free) which can be pushed/popped
 * to/from in a lock-free manner.
 *
 *
 * === Allocation ===
 *
 * The allocator (as implemented by nonmovingAllocate) starts by identifying
 * which allocator the request should be made against. It then allocates into
 * its local current segment and bumps the next_free pointer to point to the
 * next unallocated block (as indicated by the bitmap). If it finds the current
 * segment is now full it moves it to the filled list and looks for a new
 * segment to make current from a few sources:
 *
 *  1. the allocator's active list (see pop_active_segment)
 *  2. the nonmoving heap's free block pool (see nonmovingPopFreeSegment)
 *  3. allocate a new segment from the block allocator (see
 *     nonmovingAllocSegment)
 *
 * Note that allocation does *not* involve modifying the bitmap. The bitmap is
 * only modified by the collector.
 *
 *
 * === Snapshot invariant ===
 *
 * To safely collect in a concurrent setting, the collector relies on the
 * notion of a *snapshot*. The snapshot is a hypothetical frozen state of the
 * heap topology taken at the beginning of the major collection cycle.
 * With this definition we require the following property of the mark phase,
 * which we call the *snapshot invariant*,
 *
 *     All objects that were reachable at the time the snapshot was collected
 *     must have their mark bits set at the end of the mark phase.
 *
 * As the mutator might change the topology of the heap while we are marking
 * this property requires some cooperation from the mutator to maintain.
 * Specifically, we rely on a write barrier as described in Note [Update
 * remembered set].
 *
 * To determine which objects were existent when the snapshot was taken we
 * record a snapshot of each segments next_free pointer at the beginning of
 * collection.
 *
 *
 * === Collection ===
 *
 * Collection happens in a few phases some of which occur during a
 * stop-the-world period (marked with [STW]) and others which can occur
 * concurrently with mutation and minor collection (marked with [CONC]):
 *
 *  1. [STW] Preparatory GC: Here we do a standard minor collection of the
 *     younger generations (which may evacuate things to the nonmoving heap).
 *     References from younger generations into the nonmoving heap are recorded
 *     in the mark queue (see Note [Aging under the non-moving collector] in
 *     this file).
 *
 *  2. [STW] Snapshot update: Here we update the segment snapshot metadata
 *     (see nonmovingPrepareMark) and move the filled segments to
 *     nonmovingHeap.sweep_list, which is the set of segments which we will
 *     sweep this GC cycle.
 *
 *  3. [STW] Root collection: Here we walk over a variety of root sources
 *     and add them to the mark queue (see nonmovingCollect).
 *
 *  4. [CONC] Concurrent marking: Here we do the majority of marking concurrently
 *     with mutator execution (but with the write barrier enabled; see
 *     Note [Update remembered set]).
 *
 *  5. [STW] Final sync: Here we interrupt the mutators, ask them to
 *     flush their final update remembered sets, and mark any new references
 *     we find.
 *
 *  6. [CONC] Sweep: Here we walk over the nonmoving segments on sweep_list
 *     and place them back on either the active, current, or filled list,
 *     depending upon how much live data they contain.
 *
 *
 * === Marking ===
 *
 * Ignoring large and static objects, marking a closure is fairly
 * straightforward (implemented in NonMovingMark.c:mark_closure):
 *
 *  1. Check whether the closure is in the non-moving generation; if not then
 *     we ignore it.
 *  2. Find the segment containing the closure's block.
 *  3. Check whether the closure's block is above $seg->next_free_snap; if so
 *     then the block was not allocated when we took the snapshot and therefore
 *     we don't need to mark it.
 *  4. Check whether the block's bitmap bits is equal to nonmovingMarkEpoch. If
 *     so then we can stop as we have already marked it.
 *  5. Push the closure's pointers to the mark queue.
 *  6. Set the blocks bitmap bits to nonmovingMarkEpoch.
 *
 * Note that the ordering of (5) and (6) is rather important, as described in
 * Note [StgStack dirtiness flags and concurrent marking].
 *
 *
 * === Other references ===
 *
 * Apart from the design document in docs/storage/nonmoving-gc and the Ueno
 * 2016 paper (TODO citation) from which it drew inspiration, there are a
 * variety of other relevant Notes scattered throughout the tree:
 *
 *  - Note [Concurrent non-moving collection] (NonMoving.c) describes
 *    concurrency control of the nonmoving collector
 *
 *  - Note [Live data accounting in nonmoving collector] (NonMoving.c)
 *    describes how we track the quantity of live data in the nonmoving
 *    generation.
 *
 *  - Note [Aging under the non-moving collector] (NonMoving.c) describes how
 *    we accommodate aging
 *
 *  - Note [Large objects in the non-moving collector] (NonMovingMark.c)
 *    describes how we track large objects.
 *
 *  - Note [Update remembered set] (NonMovingMark.c) describes the function and
 *    implementation of the update remembered set used to realize the concurrent
 *    write barrier.
 *
 *  - Note [Concurrent read barrier on deRefWeak#] (NonMovingMark.c) describes
 *    the read barrier on Weak# objects.
 *
 *  - Note [Unintentional marking in resurrectThreads] (NonMovingMark.c) describes
 *    a tricky interaction between the update remembered set flush and weak
 *    finalization.
 *
 *  - Note [Origin references in the nonmoving collector] (NonMovingMark.h)
 *    describes how we implement indirection short-cutting and the selector
 *    optimisation.
 *
 *  - Note [StgStack dirtiness flags and concurrent marking] (TSO.h) describes
 *    the protocol for concurrent marking of stacks.
 *
 *  - Note [Static objects under the nonmoving collector] (Storage.c) describes
 *    treatment of static objects.
 *
 *  - Note [Dirty flags in the non-moving collector] (NonMoving.c) describes
 *    how we use the DIRTY flags associated with MUT_VARs and TVARs to improve
 *    barrier efficiency.
 *
 *
 * Note [Concurrent non-moving collection]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Concurrency-control of non-moving garbage collection is a bit tricky. There
 * are a few things to keep in mind:
 *
 *  - Only one non-moving collection may be active at a time. This is enforced by the
 *    concurrent_coll_running flag, which is set when a collection is on-going. If
 *    we attempt to initiate a new collection while this is set we wait on the
 *    concurrent_coll_finished condition variable, which signals when the
 *    active collection finishes.
 *
 *  - In between the mark and sweep phases the non-moving collector must synchronize
 *    with mutator threads to collect and mark their final update remembered
 *    sets. This is accomplished using
 *    stopAllCapabilitiesWith(SYNC_FLUSH_UPD_REM_SET). Capabilities are held
 *    the final mark has concluded.
 *
 * Note that possibility of concurrent minor and non-moving collections
 * requires that we handle static objects a bit specially. See
 * Note [Static objects under the nonmoving collector] in Storage.c
 * for details.
 *
 *
 * Note [Aging under the non-moving collector]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *
 * The initial design of the non-moving collector mandated that all live data
 * be evacuated to the non-moving heap prior to a major collection. This
 * simplified certain bits of implementation and eased reasoning. However, it
 * was (unsurprisingly) also found to result in significant amounts of
 * unnecessary copying.
 *
 * Consequently, we now allow aging. Aging allows the preparatory GC leading up
 * to a major collection to evacuate some objects into the young generation.
 * However, this introduces the following tricky case that might arise after
 * we have finished the preparatory GC:
 *
 *       moving heap  ┆  non-moving heap
 *     ───────────────┆──────────────────
 *                    ┆
 *      B ←────────────── A ←─────────────── root
 *      │             ┆     ↖─────────────── gen1 mut_list
 *      ╰───────────────→ C
 *                    ┆
 *
 * In this case C is clearly live, but the non-moving collector can only see
 * this by walking through B, which lives in the moving heap. However, doing so
 * would require that we synchronize with the mutator/minor GC to ensure that it
 * isn't in the middle of moving B. What to do?
 *
 * The solution we use here is to teach the preparatory moving collector to
 * "evacuate" objects it encounters in the non-moving heap by adding them to
 * the mark queue. This is implemented by pushing the object to the update
 * remembered set of the capability held by the evacuating gc_thread
 * (implemented by markQueuePushClosureGC)
 *
 * Consequently collection of the case above would proceed as follows:
 *
 *  1. Initial state:
 *      * A lives in the non-moving heap and is reachable from the root set
 *      * A is on the oldest generation's mut_list, since it contains a pointer
 *        to B, which lives in a younger generation
 *      * B lives in the moving collector's from space
 *      * C lives in the non-moving heap
 *
 *  2. Preparatory GC: Scavenging mut_lists:
 *
 *     The mut_list of the oldest generation is scavenged, resulting in B being
 *     evacuated (aged) into the moving collector's to-space.
 *
 *  3. Preparatory GC: Scavenge B
 *
 *     B (now in to-space) is scavenged, resulting in evacuation of C.
 *     evacuate(C) pushes a reference to C to the mark queue.
 *
 *  4. Non-moving GC: C is marked
 *
 *     The non-moving collector will come to C in the mark queue and mark it.
 *
 *
 * Note [Deadlock detection under the non-moving collector]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * In GHC the garbage collector is responsible for identifying deadlocked
 * programs. Providing for this responsibility is slightly tricky in the
 * non-moving collector due to the existence of aging. In particular, the
 * non-moving collector cannot traverse objects living in a young generation
 * but reachable from the non-moving generation, as described in Note [Aging
 * under the non-moving collector].
 *
 * However, this can pose trouble for deadlock detection since it means that we
 * may conservatively mark dead closures as live. Consider this case:
 *
 *       moving heap  ┆  non-moving heap
 *     ───────────────┆──────────────────
 *                    ┆
 *      MVAR_QUEUE ←───── TSO ←───────────── gen1 mut_list
 *        ↑  │  ╰────────↗  │
 *        │  │        ┆     │
 *        │  │        ┆     ↓
 *        │  ╰──────────→ MVAR
 *        ╰─────────────────╯
 *                    ┆
 *
 * In this case we have a TSO blocked on a dead MVar. Because the MVAR_TSO_QUEUE on
 * which it is blocked lives in the moving heap, the TSO is necessarily on the
 * oldest generation's mut_list. As in Note [Aging under the non-moving
 * collector], the MVAR_TSO_QUEUE will be evacuated. If MVAR_TSO_QUEUE is aged
 * (e.g. evacuated to the young generation) then the MVAR will be added to the
 * mark queue. Consequently, we will falsely conclude that the MVAR is still
 * alive and fail to spot the deadlock.
 *
 * To avoid this sort of situation we disable aging when we are starting a
 * major GC specifically for deadlock detection (as done by
 * scheduleDetectDeadlock). This condition is recorded by the
 * deadlock_detect_gc global variable declared in GC.h. Setting this has a few
 * effects on the preparatory GC:
 *
 *  - Evac.c:alloc_for_copy forces evacuation to the non-moving generation.
 *
 *  - The evacuation logic usually responsible for pushing objects living in
 *    the non-moving heap to the mark queue is disabled. This is safe because
 *    we know that all live objects will be in the non-moving heap by the end
 *    of the preparatory moving collection.
 *
 *
 * Note [Live data accounting in nonmoving collector]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * The nonmoving collector uses an approximate heuristic for reporting live
 * data quantity. Specifically, during mark we record how much live data we
 * find in nonmoving_live_words. At the end of mark we declare this amount to
 * be how much live data we have on in the nonmoving heap (by setting
 * oldest_gen->live_estimate).
 *
 * In addition, we update oldest_gen->live_estimate every time we fill a
 * segment. This, as well, is quite approximate: we assume that all blocks
 * above next_free_next are newly-allocated. In principle we could refer to the
 * bitmap to count how many blocks we actually allocated but this too would be
 * approximate due to concurrent collection and ultimately seems more costly
 * than the problem demands.
 *
 *
 * Note [Spark management under the nonmoving collector]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Every GC, both minor and major, prunes the spark queue (using
 * Sparks.c:pruneSparkQueue) of sparks which are no longer reachable.
 * Doing this with concurrent collection is a tad subtle since the minor
 * collections cannot rely on the mark bitmap to accurately reflect the
 * reachability of a spark.
 *
 * We use a conservative reachability approximation:
 *
 *  - Minor collections assume that all sparks living in the non-moving heap
 *    are reachable.
 *
 *  - Major collections prune the spark queue during the final sync. This pruning
 *    assumes that all sparks in the young generations are reachable (since the
 *    BF_EVACUATED flag won't be set on the nursery blocks) and will consequently
 *    only prune dead sparks living in the non-moving heap.
 *
 *
 * Note [Dirty flags in the non-moving collector]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Some mutable object types (e.g. MUT_VARs, TVARs) have a one-bit dirty flag
 * encoded in their info table pointer. The moving collector uses this flag
 * to minimize redundant mut_list entries. The flag is preserves the following
 * simple invariant:
 *
 *     An object being marked as dirty implies that the object is on mut_list.
 *
 * This allows a nice optimisation in the write barrier (e.g. dirty_MUT_VAR):
 * if we write to an already-dirty object there is no need to
 * push it to the mut_list as we know it's already there.
 *
 * During GC (scavenging) we will then keep track of whether all of the
 * object's reference have been promoted. If so we can mark the object as clean.
 * If not then we re-add it to mut_list and mark it as dirty.
 *
 * In the non-moving collector we use the same dirty flag to implement a
 * related optimisation on the non-moving write barrier: Specifically, the
 * snapshot invariant only requires that the non-moving write barrier applies
 * to the *first* mutation to an object after collection begins. To achieve this,
 * we impose the following invariant:
 *
 *     An object being marked as dirty implies that all of its fields are on
 *     the mark queue (or, equivalently, update remembered set).
 *
 * With this guarantee we can safely make the the write barriers dirty objects
 * no-ops. We perform this optimisation for the following object types:
 *
 *  - MVAR
 *  - TVAR
 *  - MUT_VAR
 *
 * However, maintaining this invariant requires great care. For instance,
 * consider the case of an MVar (which has two pointer fields) before
 * preparatory collection:
 *
 *    Non-moving heap     ┊      Moving heap
 *         gen 1          ┊         gen 0
 *  ──────────────────────┼────────────────────────────────
 *                        ┊
 *         MVAR A  ────────────────→ X
 *        (dirty)  ───────────╮
 *                        ┊   ╰────→ Y
 *                        ┊          │
 *                        ┊          │
 *           ╭───────────────────────╯
 *           │            ┊
 *           ↓            ┊
 *           Z            ┊
 *                        ┊
 *
 * During the preparatory collection we promote Y to the nonmoving heap but
 * fail to promote X. Since the failed_to_evac field is conservative (being set
 * if *any* of the fields are not promoted), this gives us:
 *
 *    Non-moving heap     ┊      Moving heap
 *         gen 1          ┊         gen 0
 *  ──────────────────────┼────────────────────────────────
 *                        ┊
 *         MVAR A  ────────────────→ X
 *        (dirty)         ┊
 *           │            ┊
 *           │            ┊
 *           ↓            ┊
 *           Y            ┊
 *           │            ┊
 *           │            ┊
 *           ↓            ┊
 *           Z            ┊
 *                        ┊
 *
 * This is bad. When we resume mutation a mutator may mutate MVAR A; since it's
 * already dirty we would fail to add Y to the update remembered set, breaking the
 * snapshot invariant and potentially losing track of the liveness of Z.
 *
 * To avoid this nonmovingScavengeOne we eagerly pushes the values of the
 * fields of all objects which it fails to evacuate (e.g. MVAR A) to the update
 * remembered set during the preparatory GC. This allows us to safely skip the
 * non-moving write barrier without jeopardizing the snapshot invariant.
 *
 */

memcount nonmoving_live_words = 0;

#if defined(THREADED_RTS)
static void* nonmovingConcurrentMark(void *mark_queue);
#endif
static void nonmovingMark_(MarkQueue *mark_queue, StgWeak **dead_weaks, StgTSO **resurrected_threads);

static void nonmovingInitSegment(struct NonmovingSegment *seg, uint8_t log_block_size)
{
    bdescr *bd = Bdescr((P_) seg);
    seg->link = NULL;
    seg->todo_link = NULL;
    seg->next_free = 0;
    bd->nonmoving_segment.log_block_size = log_block_size;
    bd->nonmoving_segment.next_free_snap = 0;
    bd->u.scan = nonmovingSegmentGetBlock(seg, 0);
    nonmovingClearBitmap(seg);
}

// Add a segment to the free list.
void nonmovingPushFreeSegment(struct NonmovingSegment *seg)
{
    // See Note [Live data accounting in nonmoving collector].
    if (nonmovingHeap.n_free > NONMOVING_MAX_FREE) {
        bdescr *bd = Bdescr((StgPtr) seg);
        ACQUIRE_SM_LOCK;
        ASSERT(oldest_gen->n_blocks >= bd->blocks);
        ASSERT(oldest_gen->n_words >= BLOCK_SIZE_W * bd->blocks);
        oldest_gen->n_blocks -= bd->blocks;
        oldest_gen->n_words  -= BLOCK_SIZE_W * bd->blocks;
        freeGroup(bd);
        RELEASE_SM_LOCK;
        return;
    }

    while (true) {
        struct NonmovingSegment *old = nonmovingHeap.free;
        seg->link = old;
        if (cas((StgVolatilePtr) &nonmovingHeap.free, (StgWord) old, (StgWord) seg) == (StgWord) old)
            break;
    }
    __sync_add_and_fetch(&nonmovingHeap.n_free, 1);
}

static struct NonmovingSegment *nonmovingPopFreeSegment(void)
{
    while (true) {
        struct NonmovingSegment *seg = nonmovingHeap.free;
        if (seg == NULL) {
            return NULL;
        }
        if (cas((StgVolatilePtr) &nonmovingHeap.free,
                (StgWord) seg,
                (StgWord) seg->link) == (StgWord) seg) {
            __sync_sub_and_fetch(&nonmovingHeap.n_free, 1);
            return seg;
        }
    }
}

unsigned int nonmovingBlockCountFromSize(uint8_t log_block_size)
{
  // We compute the overwhelmingly common size cases directly to avoid a very
  // expensive integer division.
  switch (log_block_size) {
    case 3:  return nonmovingBlockCount(3);
    case 4:  return nonmovingBlockCount(4);
    case 5:  return nonmovingBlockCount(5);
    case 6:  return nonmovingBlockCount(6);
    case 7:  return nonmovingBlockCount(7);
    default: return nonmovingBlockCount(log_block_size);
  }
}

/*
 * Request a fresh segment from the free segment list or allocate one of the
 * given node.
 *
 * Caller must hold SM_MUTEX (although we take the gc_alloc_block_sync spinlock
 * under the assumption that we are in a GC context).
 */
static struct NonmovingSegment *nonmovingAllocSegment(uint32_t node)
{
    // First try taking something off of the free list
    struct NonmovingSegment *ret;
    ret = nonmovingPopFreeSegment();

    // Nothing in the free list, allocate a new segment...
    if (ret == NULL) {
        // Take gc spinlock: another thread may be scavenging a moving
        // generation and call `todo_block_full`
        ACQUIRE_SPIN_LOCK(&gc_alloc_block_sync);
        bdescr *bd = allocAlignedGroupOnNode(node, NONMOVING_SEGMENT_BLOCKS);
        // See Note [Live data accounting in nonmoving collector].
        oldest_gen->n_blocks += bd->blocks;
        oldest_gen->n_words  += BLOCK_SIZE_W * bd->blocks;
        RELEASE_SPIN_LOCK(&gc_alloc_block_sync);

        for (StgWord32 i = 0; i < bd->blocks; ++i) {
            initBdescr(&bd[i], oldest_gen, oldest_gen);
            bd[i].flags = BF_NONMOVING;
        }
        ret = (struct NonmovingSegment *)bd->start;
    }

    // Check alignment
    ASSERT(((uintptr_t)ret % NONMOVING_SEGMENT_SIZE) == 0);
    return ret;
}

static inline unsigned long log2_ceil(unsigned long x)
{
    return (sizeof(unsigned long)*8) - __builtin_clzl(x-1);
}

// Advance a segment's next_free pointer. Returns true if segment if full.
static bool advance_next_free(struct NonmovingSegment *seg, const unsigned int blk_count)
{
    const uint8_t *bitmap = seg->bitmap;
    ASSERT(blk_count == nonmovingSegmentBlockCount(seg));
#if defined(NAIVE_ADVANCE_FREE)
    // reference implementation
    for (unsigned int i = seg->next_free+1; i < blk_count; i++) {
        if (!bitmap[i]) {
            seg->next_free = i;
            return false;
        }
    }
    seg->next_free = blk_count;
    return true;
#else
    const uint8_t *c = memchr(&bitmap[seg->next_free+1], 0, blk_count - seg->next_free - 1);
    if (c == NULL) {
        seg->next_free = blk_count;
        return true;
    } else {
        seg->next_free = c - bitmap;
        return false;
    }
#endif
}

static struct NonmovingSegment *pop_active_segment(struct NonmovingAllocator *alloca)
{
    while (true) {
        struct NonmovingSegment *seg = alloca->active;
        if (seg == NULL) {
            return NULL;
        }
        if (cas((StgVolatilePtr) &alloca->active,
                (StgWord) seg,
                (StgWord) seg->link) == (StgWord) seg) {
            return seg;
        }
    }
}

/* Allocate a block in the nonmoving heap. Caller must hold SM_MUTEX. sz is in words */
GNUC_ATTR_HOT
void *nonmovingAllocate(Capability *cap, StgWord sz)
{
    unsigned int log_block_size = log2_ceil(sz * sizeof(StgWord));
    unsigned int block_count = nonmovingBlockCountFromSize(log_block_size);

    // The max we ever allocate is 3276 bytes (anything larger is a large
    // object and not moved) which is covered by allocator 9.
    ASSERT(log_block_size < NONMOVING_ALLOCA0 + NONMOVING_ALLOCA_CNT);

    struct NonmovingAllocator *alloca = nonmovingHeap.allocators[log_block_size - NONMOVING_ALLOCA0];

    // Allocate into current segment
    struct NonmovingSegment *current = alloca->current[cap->no];
    ASSERT(current); // current is never NULL
    void *ret = nonmovingSegmentGetBlock_(current, log_block_size, current->next_free);
    ASSERT(GET_CLOSURE_TAG(ret) == 0); // check alignment

    // Advance the current segment's next_free or allocate a new segment if full
    bool full = advance_next_free(current, block_count);
    if (full) {
        // Current segment is full: update live data estimate link it to
        // filled, take an active segment if one exists, otherwise allocate a
        // new segment.

        // Update live data estimate.
        // See Note [Live data accounting in nonmoving collector].
        unsigned int new_blocks = block_count - nonmovingSegmentInfo(current)->next_free_snap;
        unsigned int block_size = 1 << log_block_size;
        atomic_inc(&oldest_gen->live_estimate, new_blocks * block_size / sizeof(W_));

        // push the current segment to the filled list
        nonmovingPushFilledSegment(current);

        // first look for a new segment in the active list
        struct NonmovingSegment *new_current = pop_active_segment(alloca);

        // there are no active segments, allocate new segment
        if (new_current == NULL) {
            new_current = nonmovingAllocSegment(cap->node);
            nonmovingInitSegment(new_current, log_block_size);
        }

        // make it current
        new_current->link = NULL;
        alloca->current[cap->no] = new_current;
    }

    return ret;
}

/* Allocate a nonmovingAllocator */
static struct NonmovingAllocator *alloc_nonmoving_allocator(uint32_t n_caps)
{
    size_t allocator_sz =
        sizeof(struct NonmovingAllocator) +
        sizeof(void*) * n_caps; // current segment pointer for each capability
    struct NonmovingAllocator *alloc =
        stgMallocBytes(allocator_sz, "nonmovingInit");
    memset(alloc, 0, allocator_sz);
    return alloc;
}

static void free_nonmoving_allocator(struct NonmovingAllocator *alloc)
{
    stgFree(alloc);
}

void nonmovingInit(void)
{
    if (! RtsFlags.GcFlags.useNonmoving) return;
#if defined(THREADED_RTS)
    initMutex(&nonmoving_collection_mutex);
    initCondition(&concurrent_coll_finished);
    initMutex(&concurrent_coll_finished_lock);
#endif
    for (unsigned int i = 0; i < NONMOVING_ALLOCA_CNT; i++) {
        nonmovingHeap.allocators[i] = alloc_nonmoving_allocator(n_capabilities);
    }
    nonmovingMarkInitUpdRemSet();
}

// Stop any nonmoving collection in preparation for RTS shutdown.
void nonmovingStop(void)
{
    if (! RtsFlags.GcFlags.useNonmoving) return;
#if defined(THREADED_RTS)
    if (mark_thread) {
        debugTrace(DEBUG_nonmoving_gc,
                   "waiting for nonmoving collector thread to terminate");
        ACQUIRE_LOCK(&concurrent_coll_finished_lock);
        waitCondition(&concurrent_coll_finished, &concurrent_coll_finished_lock);
    }
#endif
}

void nonmovingExit(void)
{
    if (! RtsFlags.GcFlags.useNonmoving) return;

    // First make sure collector is stopped before we tear things down.
    nonmovingStop();

#if defined(THREADED_RTS)
    closeMutex(&concurrent_coll_finished_lock);
    closeCondition(&concurrent_coll_finished);
    closeMutex(&nonmoving_collection_mutex);
#endif

    for (unsigned int i = 0; i < NONMOVING_ALLOCA_CNT; i++) {
        free_nonmoving_allocator(nonmovingHeap.allocators[i]);
    }
}

/*
 * Assumes that no garbage collector or mutator threads are running to safely
 * resize the nonmoving_allocators.
 *
 * Must hold sm_mutex.
 */
void nonmovingAddCapabilities(uint32_t new_n_caps)
{
    unsigned int old_n_caps = nonmovingHeap.n_caps;
    struct NonmovingAllocator **allocs = nonmovingHeap.allocators;

    for (unsigned int i = 0; i < NONMOVING_ALLOCA_CNT; i++) {
        struct NonmovingAllocator *old = allocs[i];
        allocs[i] = alloc_nonmoving_allocator(new_n_caps);

        // Copy the old state
        allocs[i]->filled = old->filled;
        allocs[i]->active = old->active;
        for (unsigned int j = 0; j < old_n_caps; j++) {
            allocs[i]->current[j] = old->current[j];
        }
        stgFree(old);

        // Initialize current segments for the new capabilities
        for (unsigned int j = old_n_caps; j < new_n_caps; j++) {
            allocs[i]->current[j] = nonmovingAllocSegment(capabilities[j]->node);
            nonmovingInitSegment(allocs[i]->current[j], NONMOVING_ALLOCA0 + i);
            allocs[i]->current[j]->link = NULL;
        }
    }
    nonmovingHeap.n_caps = new_n_caps;
}

void nonmovingClearBitmap(struct NonmovingSegment *seg)
{
    unsigned int n = nonmovingSegmentBlockCount(seg);
    memset(seg->bitmap, 0, n);
}

/* Prepare the heap bitmaps and snapshot metadata for a mark */
static void nonmovingPrepareMark(void)
{
    // See Note [Static objects under the nonmoving collector].
    prev_static_flag = static_flag;
    static_flag =
        static_flag == STATIC_FLAG_A ? STATIC_FLAG_B : STATIC_FLAG_A;

    // Should have been cleared by the last sweep
    ASSERT(nonmovingHeap.sweep_list == NULL);

    nonmovingBumpEpoch();
    for (int alloca_idx = 0; alloca_idx < NONMOVING_ALLOCA_CNT; ++alloca_idx) {
        struct NonmovingAllocator *alloca = nonmovingHeap.allocators[alloca_idx];

        // Update current segments' snapshot pointers
        for (uint32_t cap_n = 0; cap_n < n_capabilities; ++cap_n) {
            struct NonmovingSegment *seg = alloca->current[cap_n];
            nonmovingSegmentInfo(seg)->next_free_snap = seg->next_free;
        }

        // Save the filled segments for later processing during the concurrent
        // mark phase.
        alloca->saved_filled = alloca->filled;
        alloca->filled = NULL;

        // N.B. It's not necessary to update snapshot pointers of active segments;
        // they were set after they were swept and haven't seen any allocation
        // since.
    }

    // Clear large object bits of existing large objects
    for (bdescr *bd = nonmoving_large_objects; bd; bd = bd->link) {
        bd->flags &= ~BF_MARKED;
    }

    // Add newly promoted large objects and clear mark bits
    bdescr *next;
    ASSERT(oldest_gen->scavenged_large_objects == NULL);
    for (bdescr *bd = oldest_gen->large_objects; bd; bd = next) {
        next = bd->link;
        bd->flags |= BF_NONMOVING_SWEEPING;
        bd->flags &= ~BF_MARKED;
        dbl_link_onto(bd, &nonmoving_large_objects);
    }
    n_nonmoving_large_blocks += oldest_gen->n_large_blocks;
    oldest_gen->large_objects = NULL;
    oldest_gen->n_large_words = 0;
    oldest_gen->n_large_blocks = 0;
    nonmoving_live_words = 0;

    // Clear compact object mark bits
    for (bdescr *bd = nonmoving_compact_objects; bd; bd = bd->link) {
        bd->flags &= ~BF_MARKED;
    }

    // Move new compact objects from younger generations to nonmoving_compact_objects
    for (bdescr *bd = oldest_gen->compact_objects; bd; bd = next) {
        next = bd->link;
        bd->flags |= BF_NONMOVING_SWEEPING;
        bd->flags &= ~BF_MARKED;
        dbl_link_onto(bd, &nonmoving_compact_objects);
    }
    n_nonmoving_compact_blocks += oldest_gen->n_compact_blocks;
    oldest_gen->n_compact_blocks = 0;
    oldest_gen->compact_objects = NULL;
    // TODO (osa): what about "in import" stuff??



#if defined(DEBUG)
    debug_caf_list_snapshot = debug_caf_list;
    debug_caf_list = (StgIndStatic*)END_OF_CAF_LIST;
#endif
}

// Mark weak pointers in the non-moving heap. They'll either end up in
// dead_weak_ptr_list or stay in weak_ptr_list. Either way they need to be kept
// during sweep. See `MarkWeak.c:markWeakPtrList` for the moving heap variant
// of this.
static void nonmovingMarkWeakPtrList(MarkQueue *mark_queue, StgWeak *dead_weak_ptr_list)
{
    for (StgWeak *w = oldest_gen->weak_ptr_list; w; w = w->link) {
        markQueuePushClosure_(mark_queue, (StgClosure*)w);
        // Do not mark finalizers and values here, those fields will be marked
        // in `nonmovingMarkDeadWeaks` (for dead weaks) or
        // `nonmovingTidyWeaks` (for live weaks)
    }

    // We need to mark dead_weak_ptr_list too. This is subtle:
    //
    // - By the beginning of this GC we evacuated all weaks to the non-moving
    //   heap (in `markWeakPtrList`)
    //
    // - During the scavenging of the moving heap we discovered that some of
    //   those weaks are dead and moved them to `dead_weak_ptr_list`. Note that
    //   because of the fact above _all weaks_ are in the non-moving heap at
    //   this point.
    //
    // - So, to be able to traverse `dead_weak_ptr_list` and run finalizers we
    //   need to mark it.
    for (StgWeak *w = dead_weak_ptr_list; w; w = w->link) {
        markQueuePushClosure_(mark_queue, (StgClosure*)w);
        nonmovingMarkDeadWeak(mark_queue, w);
    }
}

void nonmovingCollect(StgWeak **dead_weaks, StgTSO **resurrected_threads)
{
#if defined(THREADED_RTS)
    // We can't start a new collection until the old one has finished
    // We also don't run in final GC
    if (concurrent_coll_running || sched_state > SCHED_RUNNING) {
        return;
    }
#endif

    trace(TRACE_nonmoving_gc, "Starting nonmoving GC preparation");
    resizeGenerations();

    nonmovingPrepareMark();

    // N.B. These should have been cleared at the end of the last sweep.
    ASSERT(nonmoving_marked_large_objects == NULL);
    ASSERT(n_nonmoving_marked_large_blocks == 0);
    ASSERT(nonmoving_marked_compact_objects == NULL);
    ASSERT(n_nonmoving_marked_compact_blocks == 0);

    MarkQueue *mark_queue = stgMallocBytes(sizeof(MarkQueue), "mark queue");
    initMarkQueue(mark_queue);
    current_mark_queue = mark_queue;

    // Mark roots
    trace(TRACE_nonmoving_gc, "Marking roots for nonmoving GC");
    markCAFs((evac_fn)markQueueAddRoot, mark_queue);
    for (unsigned int n = 0; n < n_capabilities; ++n) {
        markCapability((evac_fn)markQueueAddRoot, mark_queue,
                capabilities[n], true/*don't mark sparks*/);
    }
    markScheduler((evac_fn)markQueueAddRoot, mark_queue);
    nonmovingMarkWeakPtrList(mark_queue, *dead_weaks);
    markStablePtrTable((evac_fn)markQueueAddRoot, mark_queue);

    // Mark threads resurrected during moving heap scavenging
    for (StgTSO *tso = *resurrected_threads; tso != END_TSO_QUEUE; tso = tso->global_link) {
        markQueuePushClosure_(mark_queue, (StgClosure*)tso);
    }
    trace(TRACE_nonmoving_gc, "Finished marking roots for nonmoving GC");

    // Roots marked, mark threads and weak pointers

    // At this point all threads are moved to threads list (from old_threads)
    // and all weaks are moved to weak_ptr_list (from old_weak_ptr_list) by
    // the previous scavenge step, so we need to move them to "old" lists
    // again.

    // Fine to override old_threads because any live or resurrected threads are
    // moved to threads or resurrected_threads lists.
    ASSERT(oldest_gen->old_threads == END_TSO_QUEUE);
    ASSERT(nonmoving_old_threads == END_TSO_QUEUE);
    nonmoving_old_threads = oldest_gen->threads;
    oldest_gen->threads = END_TSO_QUEUE;

    // Make sure we don't lose any weak ptrs here. Weaks in old_weak_ptr_list
    // will either be moved to `dead_weaks` (if dead) or `weak_ptr_list` (if
    // alive).
    ASSERT(oldest_gen->old_weak_ptr_list == NULL);
    ASSERT(nonmoving_old_weak_ptr_list == NULL);
    nonmoving_old_weak_ptr_list = oldest_gen->weak_ptr_list;
    oldest_gen->weak_ptr_list = NULL;
    trace(TRACE_nonmoving_gc, "Finished nonmoving GC preparation");

    // We are now safe to start concurrent marking

    // Note that in concurrent mark we can't use dead_weaks and
    // resurrected_threads from the preparation to add new weaks and threads as
    // that would cause races between minor collection and mark. So we only pass
    // those lists to mark function in sequential case. In concurrent case we
    // allocate fresh lists.

#if defined(THREADED_RTS)
    // If we're interrupting or shutting down, do not let this capability go and
    // run a STW collection. Reason: we won't be able to acquire this capability
    // again for the sync if we let it go, because it'll immediately start doing
    // a major GC, because that's what we do when exiting scheduler (see
    // exitScheduler()).
    if (sched_state == SCHED_RUNNING) {
        concurrent_coll_running = true;
        nonmoving_write_barrier_enabled = true;
        debugTrace(DEBUG_nonmoving_gc, "Starting concurrent mark thread");
        if (createOSThread(&mark_thread, "non-moving mark thread",
                           nonmovingConcurrentMark, mark_queue) != 0) {
            barf("nonmovingCollect: failed to spawn mark thread: %s", strerror(errno));
        }
    } else {
        nonmovingConcurrentMark(mark_queue);
    }
#else
    // Use the weak and thread lists from the preparation for any new weaks and
    // threads found to be dead in mark.
    nonmovingMark_(mark_queue, dead_weaks, resurrected_threads);
#endif
}

/* Mark mark queue, threads, and weak pointers until no more weaks have been
 * resuscitated
 */
static void nonmovingMarkThreadsWeaks(MarkQueue *mark_queue)
{
    while (true) {
        // Propagate marks
        nonmovingMark(mark_queue);

        // Tidy threads and weaks
        nonmovingTidyThreads();

        if (! nonmovingTidyWeaks(mark_queue))
            return;
    }
}

#if defined(THREADED_RTS)
static void* nonmovingConcurrentMark(void *data)
{
    MarkQueue *mark_queue = (MarkQueue*)data;
    StgWeak *dead_weaks = NULL;
    StgTSO *resurrected_threads = (StgTSO*)&stg_END_TSO_QUEUE_closure;
    nonmovingMark_(mark_queue, &dead_weaks, &resurrected_threads);
    return NULL;
}

// TODO: Not sure where to put this function.
// Append w2 to the end of w1.
static void appendWeakList( StgWeak **w1, StgWeak *w2 )
{
    while (*w1) {
        w1 = &(*w1)->link;
    }
    *w1 = w2;
}
#endif

static void nonmovingMark_(MarkQueue *mark_queue, StgWeak **dead_weaks, StgTSO **resurrected_threads)
{
    ACQUIRE_LOCK(&nonmoving_collection_mutex);
    debugTrace(DEBUG_nonmoving_gc, "Starting mark...");
    stat_startNonmovingGc();

    // Walk the list of filled segments that we collected during preparation,
    // updated their snapshot pointers and move them to the sweep list.
    for (int alloca_idx = 0; alloca_idx < NONMOVING_ALLOCA_CNT; ++alloca_idx) {
        struct NonmovingSegment *filled = nonmovingHeap.allocators[alloca_idx]->saved_filled;
        uint32_t n_filled = 0;
        if (filled) {
            struct NonmovingSegment *seg = filled;
            while (true) {
                // Set snapshot
                nonmovingSegmentInfo(seg)->next_free_snap = seg->next_free;
                n_filled++;
                if (seg->link)
                    seg = seg->link;
                else
                    break;
            }
            // add filled segments to sweep_list
            seg->link = nonmovingHeap.sweep_list;
            nonmovingHeap.sweep_list = filled;
        }
    }

    // Do concurrent marking; most of the heap will get marked here.
    nonmovingMarkThreadsWeaks(mark_queue);

#if defined(THREADED_RTS)
    Task *task = newBoundTask();

    // If at this point if we've decided to exit then just return
    if (sched_state > SCHED_RUNNING) {
        // Note that we break our invariants here and leave segments in
        // nonmovingHeap.sweep_list, don't free nonmoving_large_objects etc.
        // However because we won't be running mark-sweep in the final GC this
        // is OK.

        // This is a RTS shutdown so we need to move our copy (snapshot) of
        // weaks (nonmoving_old_weak_ptr_list and nonmoving_weak_ptr_list) to
        // oldest_gen->threads to be able to run C finalizers in hs_exit_. Note
        // that there may be more weaks added to oldest_gen->threads since we
        // started mark, so we need to append our list to the tail of
        // oldest_gen->threads.
        appendWeakList(&nonmoving_old_weak_ptr_list, nonmoving_weak_ptr_list);
        appendWeakList(&oldest_gen->weak_ptr_list, nonmoving_old_weak_ptr_list);
        // These lists won't be used again so this is not necessary, but still
        nonmoving_old_weak_ptr_list = NULL;
        nonmoving_weak_ptr_list = NULL;

        goto finish;
    }

    // We're still running, request a sync
    nonmovingBeginFlush(task);

    bool all_caps_syncd;
    do {
        all_caps_syncd = nonmovingWaitForFlush();
        nonmovingMarkThreadsWeaks(mark_queue);
    } while (!all_caps_syncd);
#endif

    nonmovingResurrectThreads(mark_queue, resurrected_threads);

    // No more resurrecting threads after this point

    // Do last marking of weak pointers
    while (true) {
        // Propagate marks
        nonmovingMark(mark_queue);

        if (!nonmovingTidyWeaks(mark_queue))
            break;
    }

    nonmovingMarkDeadWeaks(mark_queue, dead_weaks);

    // Propagate marks
    nonmovingMark(mark_queue);

    // Now remove all dead objects from the mut_list to ensure that a younger
    // generation collection doesn't attempt to look at them after we've swept.
    nonmovingSweepMutLists();

    debugTrace(DEBUG_nonmoving_gc,
               "Done marking, resurrecting threads before releasing capabilities");


    // Schedule finalizers and resurrect threads
#if defined(THREADED_RTS)
    // Just pick a random capability. Not sure if this is a good idea -- we use
    // only one capability for all finalizers.
    scheduleFinalizers(capabilities[0], *dead_weaks);
    // Note that this mutates heap and causes running write barriers.
    // See Note [Unintentional marking in resurrectThreads] in NonMovingMark.c
    // for how we deal with this.
    resurrectThreads(*resurrected_threads);
#endif

#if defined(DEBUG)
    // Zap CAFs that we will sweep
    nonmovingGcCafs();
#endif

    ASSERT(mark_queue->top->head == 0);
    ASSERT(mark_queue->blocks->link == NULL);

    // Update oldest_gen thread and weak lists
    // Note that we need to append these lists as a concurrent minor GC may have
    // added stuff to them while we're doing mark-sweep concurrently
    {
        StgTSO **threads = &oldest_gen->threads;
        while (*threads != END_TSO_QUEUE) {
            threads = &(*threads)->global_link;
        }
        *threads = nonmoving_threads;
        nonmoving_threads = END_TSO_QUEUE;
        nonmoving_old_threads = END_TSO_QUEUE;
    }

    {
        StgWeak **weaks = &oldest_gen->weak_ptr_list;
        while (*weaks) {
            weaks = &(*weaks)->link;
        }
        *weaks = nonmoving_weak_ptr_list;
        nonmoving_weak_ptr_list = NULL;
        nonmoving_old_weak_ptr_list = NULL;
    }

    // Prune spark lists
    // See Note [Spark management under the nonmoving collector].
#if defined(THREADED_RTS)
    for (uint32_t n = 0; n < n_capabilities; n++) {
        pruneSparkQueue(true, capabilities[n]);
    }
#endif

    // Everything has been marked; allow the mutators to proceed
#if defined(THREADED_RTS)
    nonmoving_write_barrier_enabled = false;
    nonmovingFinishFlush(task);
#endif

    current_mark_queue = NULL;
    freeMarkQueue(mark_queue);
    stgFree(mark_queue);

    oldest_gen->live_estimate = nonmoving_live_words;
    oldest_gen->n_old_blocks = 0;
    resizeGenerations();

    /****************************************************
     * Sweep
     ****************************************************/

    traceConcSweepBegin();

    // Because we can't mark large object blocks (no room for mark bit) we
    // collect them in a map in mark_queue and we pass it here to sweep large
    // objects
    nonmovingSweepLargeObjects();
    nonmovingSweepCompactObjects();
    nonmovingSweepStableNameTable();

    nonmovingSweep();
    ASSERT(nonmovingHeap.sweep_list == NULL);
    debugTrace(DEBUG_nonmoving_gc, "Finished sweeping.");
    traceConcSweepEnd();
#if defined(DEBUG)
    if (RtsFlags.DebugFlags.nonmoving_gc)
        nonmovingPrintAllocatorCensus();
#endif
#if defined(TRACING)
    if (RtsFlags.TraceFlags.nonmoving_gc)
        nonmovingTraceAllocatorCensus();
#endif

    // TODO: Remainder of things done by GarbageCollect (update stats)

#if defined(THREADED_RTS)
finish:
    boundTaskExiting(task);

    // We are done...
    mark_thread = 0;
    stat_endNonmovingGc();

    // Signal that the concurrent collection is finished, allowing the next
    // non-moving collection to proceed
    concurrent_coll_running = false;
    signalCondition(&concurrent_coll_finished);
    RELEASE_LOCK(&nonmoving_collection_mutex);
#endif
}

#if defined(DEBUG)

// Use this with caution: this doesn't work correctly during scavenge phase
// when we're doing parallel scavenging. Use it in mark phase or later (where
// we don't allocate more anymore).
void assert_in_nonmoving_heap(StgPtr p)
{
    if (!HEAP_ALLOCED_GC(p))
        return;

    bdescr *bd = Bdescr(p);
    if (bd->flags & BF_LARGE) {
        // It should be in a capability (if it's not filled yet) or in non-moving heap
        for (uint32_t cap = 0; cap < n_capabilities; ++cap) {
            if (bd == capabilities[cap]->pinned_object_block) {
                return;
            }
        }
        ASSERT(bd->flags & BF_NONMOVING);
        return;
    }

    // Search snapshot segments
    for (struct NonmovingSegment *seg = nonmovingHeap.sweep_list; seg; seg = seg->link) {
        if (p >= (P_)seg && p < (((P_)seg) + NONMOVING_SEGMENT_SIZE_W)) {
            return;
        }
    }

    for (int alloca_idx = 0; alloca_idx < NONMOVING_ALLOCA_CNT; ++alloca_idx) {
        struct NonmovingAllocator *alloca = nonmovingHeap.allocators[alloca_idx];
        // Search current segments
        for (uint32_t cap_idx = 0; cap_idx < n_capabilities; ++cap_idx) {
            struct NonmovingSegment *seg = alloca->current[cap_idx];
            if (p >= (P_)seg && p < (((P_)seg) + NONMOVING_SEGMENT_SIZE_W)) {
                return;
            }
        }

        // Search active segments
        int seg_idx = 0;
        struct NonmovingSegment *seg = alloca->active;
        while (seg) {
            if (p >= (P_)seg && p < (((P_)seg) + NONMOVING_SEGMENT_SIZE_W)) {
                return;
            }
            seg_idx++;
            seg = seg->link;
        }

        // Search filled segments
        seg_idx = 0;
        seg = alloca->filled;
        while (seg) {
            if (p >= (P_)seg && p < (((P_)seg) + NONMOVING_SEGMENT_SIZE_W)) {
                return;
            }
            seg_idx++;
            seg = seg->link;
        }
    }

    // We don't search free segments as they're unused

    barf("%p is not in nonmoving heap\n", (void*)p);
}

void nonmovingPrintSegment(struct NonmovingSegment *seg)
{
    int num_blocks = nonmovingSegmentBlockCount(seg);
    uint8_t log_block_size = nonmovingSegmentLogBlockSize(seg);

    debugBelch("Segment with %d blocks of size 2^%d (%d bytes, %u words, scan: %p)\n",
               num_blocks,
               log_block_size,
               1 << log_block_size,
               (unsigned int) ROUNDUP_BYTES_TO_WDS(1 << log_block_size),
               (void*)Bdescr((P_)seg)->u.scan);

    for (nonmoving_block_idx p_idx = 0; p_idx < seg->next_free; ++p_idx) {
        StgClosure *p = (StgClosure*)nonmovingSegmentGetBlock(seg, p_idx);
        if (nonmovingGetMark(seg, p_idx) != 0) {
            debugBelch("%d (%p)* :\t", p_idx, (void*)p);
        } else {
            debugBelch("%d (%p)  :\t", p_idx, (void*)p);
        }
        printClosure(p);
    }

    debugBelch("End of segment\n\n");
}

void nonmovingPrintAllocator(struct NonmovingAllocator *alloc)
{
    debugBelch("Allocator at %p\n", (void*)alloc);
    debugBelch("Filled segments:\n");
    for (struct NonmovingSegment *seg = alloc->filled; seg != NULL; seg = seg->link) {
        debugBelch("%p ", (void*)seg);
    }
    debugBelch("\nActive segments:\n");
    for (struct NonmovingSegment *seg = alloc->active; seg != NULL; seg = seg->link) {
        debugBelch("%p ", (void*)seg);
    }
    debugBelch("\nCurrent segments:\n");
    for (uint32_t i = 0; i < n_capabilities; ++i) {
        debugBelch("%p ", alloc->current[i]);
    }
    debugBelch("\n");
}

void locate_object(P_ obj)
{
    // Search allocators
    for (int alloca_idx = 0; alloca_idx < NONMOVING_ALLOCA_CNT; ++alloca_idx) {
        struct NonmovingAllocator *alloca = nonmovingHeap.allocators[alloca_idx];
        for (uint32_t cap = 0; cap < n_capabilities; ++cap) {
            struct NonmovingSegment *seg = alloca->current[cap];
            if (obj >= (P_)seg && obj < (((P_)seg) + NONMOVING_SEGMENT_SIZE_W)) {
                debugBelch("%p is in current segment of capability %d of allocator %d at %p\n", obj, cap, alloca_idx, (void*)seg);
                return;
            }
        }
        int seg_idx = 0;
        struct NonmovingSegment *seg = alloca->active;
        while (seg) {
            if (obj >= (P_)seg && obj < (((P_)seg) + NONMOVING_SEGMENT_SIZE_W)) {
                debugBelch("%p is in active segment %d of allocator %d at %p\n", obj, seg_idx, alloca_idx, (void*)seg);
                return;
            }
            seg_idx++;
            seg = seg->link;
        }

        seg_idx = 0;
        seg = alloca->filled;
        while (seg) {
            if (obj >= (P_)seg && obj < (((P_)seg) + NONMOVING_SEGMENT_SIZE_W)) {
                debugBelch("%p is in filled segment %d of allocator %d at %p\n", obj, seg_idx, alloca_idx, (void*)seg);
                return;
            }
            seg_idx++;
            seg = seg->link;
        }
    }

    struct NonmovingSegment *seg = nonmovingHeap.free;
    int seg_idx = 0;
    while (seg) {
        if (obj >= (P_)seg && obj < (((P_)seg) + NONMOVING_SEGMENT_SIZE_W)) {
            debugBelch("%p is in free segment %d at %p\n", obj, seg_idx, (void*)seg);
            return;
        }
        seg_idx++;
        seg = seg->link;
    }

    // Search nurseries
    for (uint32_t nursery_idx = 0; nursery_idx < n_nurseries; ++nursery_idx) {
        for (bdescr* nursery_block = nurseries[nursery_idx].blocks; nursery_block; nursery_block = nursery_block->link) {
            if (obj >= nursery_block->start && obj <= nursery_block->start + nursery_block->blocks*BLOCK_SIZE_W) {
                debugBelch("%p is in nursery %d\n", obj, nursery_idx);
                return;
            }
        }
    }

    // Search generations
    for (uint32_t g = 0; g < RtsFlags.GcFlags.generations - 1; ++g) {
        generation *gen = &generations[g];
        for (bdescr *blk = gen->blocks; blk; blk = blk->link) {
            if (obj >= blk->start && obj < blk->free) {
                debugBelch("%p is in generation %" FMT_Word32 " blocks\n", obj, g);
                return;
            }
        }
        for (bdescr *blk = gen->old_blocks; blk; blk = blk->link) {
            if (obj >= blk->start && obj < blk->free) {
                debugBelch("%p is in generation %" FMT_Word32 " old blocks\n", obj, g);
                return;
            }
        }
    }

    // Search large objects
    for (uint32_t g = 0; g < RtsFlags.GcFlags.generations - 1; ++g) {
        generation *gen = &generations[g];
        for (bdescr *large_block = gen->large_objects; large_block; large_block = large_block->link) {
            if ((P_)large_block->start == obj) {
                debugBelch("%p is in large blocks of generation %d\n", obj, g);
                return;
            }
        }
    }

    for (bdescr *large_block = nonmoving_large_objects; large_block; large_block = large_block->link) {
        if ((P_)large_block->start == obj) {
            debugBelch("%p is in nonmoving_large_objects\n", obj);
            return;
        }
    }

    for (bdescr *large_block = nonmoving_marked_large_objects; large_block; large_block = large_block->link) {
        if ((P_)large_block->start == obj) {
            debugBelch("%p is in nonmoving_marked_large_objects\n", obj);
            return;
        }
    }

    // Search workspaces FIXME only works in non-threaded runtime
#if !defined(THREADED_RTS)
    for (uint32_t g = 0; g < RtsFlags.GcFlags.generations - 1; ++ g) {
        gen_workspace *ws = &gct->gens[g];
        for (bdescr *blk = ws->todo_bd; blk; blk = blk->link) {
            if (obj >= blk->start && obj < blk->free) {
                debugBelch("%p is in generation %" FMT_Word32 " todo bds\n", obj, g);
                return;
            }
        }
        for (bdescr *blk = ws->scavd_list; blk; blk = blk->link) {
            if (obj >= blk->start && obj < blk->free) {
                debugBelch("%p is in generation %" FMT_Word32 " scavd bds\n", obj, g);
                return;
            }
        }
        for (bdescr *blk = ws->todo_large_objects; blk; blk = blk->link) {
            if (obj >= blk->start && obj < blk->free) {
                debugBelch("%p is in generation %" FMT_Word32 " todo large bds\n", obj, g);
                return;
            }
        }
    }
#endif
}

void nonmovingPrintSweepList()
{
    debugBelch("==== SWEEP LIST =====\n");
    int i = 0;
    for (struct NonmovingSegment *seg = nonmovingHeap.sweep_list; seg; seg = seg->link) {
        debugBelch("%d: %p\n", i++, (void*)seg);
    }
    debugBelch("= END OF SWEEP LIST =\n");
}

void check_in_mut_list(StgClosure *p)
{
    for (uint32_t cap_n = 0; cap_n < n_capabilities; ++cap_n) {
        for (bdescr *bd = capabilities[cap_n]->mut_lists[oldest_gen->no]; bd; bd = bd->link) {
            for (StgPtr q = bd->start; q < bd->free; ++q) {
                if (*((StgPtr**)q) == (StgPtr*)p) {
                    debugBelch("Object is in mut list of cap %d: %p\n", cap_n, capabilities[cap_n]->mut_lists[oldest_gen->no]);
                    return;
                }
            }
        }
    }

    debugBelch("Object is not in a mut list\n");
}

void print_block_list(bdescr* bd)
{
    while (bd) {
        debugBelch("%p, ", (void*)bd);
        bd = bd->link;
    }
    debugBelch("\n");
}

void print_thread_list(StgTSO* tso)
{
    while (tso != END_TSO_QUEUE) {
        printClosure((StgClosure*)tso);
        tso = tso->global_link;
    }
}

#endif
