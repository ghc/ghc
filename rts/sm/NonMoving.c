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
 *
 * TODO
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
 */

W_ nonmoving_live_words = 0;

#if defined(THREADED_RTS)
static void* nonmovingConcurrentMark(void *mark_queue);
#endif
static void nonmovingClearBitmap(struct NonmovingSegment *seg);
static void nonmovingMark_(MarkQueue *mark_queue, StgWeak **dead_weaks, StgTSO **resurrected_threads);

static void nonmovingInitSegment(struct NonmovingSegment *seg, uint8_t block_size)
{
    seg->link = NULL;
    seg->todo_link = NULL;
    seg->next_free = 0;
    seg->next_free_snap = 0;
    seg->block_size = block_size;
    nonmovingClearBitmap(seg);
    Bdescr((P_)seg)->u.scan = nonmovingSegmentGetBlock(seg, 0);
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

/*
 * Request a fresh segment from the free segment list or allocate one of the
 * given node.
 *
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

static inline unsigned long log2_floor(unsigned long x)
{
    return sizeof(unsigned long)*8 - 1 - __builtin_clzl(x);
}

static inline unsigned long log2_ceil(unsigned long x)
{
    unsigned long log = log2_floor(x);
    return (x - (1 << log)) ? log + 1 : log;
}

// Advance a segment's next_free pointer. Returns true if segment if full.
static bool advance_next_free(struct NonmovingSegment *seg)
{
    uint8_t *bitmap = seg->bitmap;
    unsigned int blk_count = nonmovingSegmentBlockCount(seg);
    for (unsigned int i = seg->next_free+1; i < blk_count; i++) {
        if (!bitmap[i]) {
            seg->next_free = i;
            return false;
        }
    }
    seg->next_free = blk_count;
    return true;
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

/* sz is in words */
GNUC_ATTR_HOT
void *nonmovingAllocate(Capability *cap, StgWord sz)
{
    unsigned int allocator_idx = log2_ceil(sz * sizeof(StgWord)) - NONMOVING_ALLOCA0;

    // The max we ever allocate is 3276 bytes (anything larger is a large
    // object and not moved) which is covered by allocator 9.
    ASSERT(allocator_idx < NONMOVING_ALLOCA_CNT);

    struct NonmovingAllocator *alloca = nonmovingHeap.allocators[allocator_idx];

    // Allocate into current segment
    struct NonmovingSegment *current = alloca->current[cap->no];
    ASSERT(current); // current is never NULL
    void *ret = nonmovingSegmentGetBlock(current, current->next_free);
    ASSERT(GET_CLOSURE_TAG(ret) == 0); // check alignment

    // Add segment to the todo list unless it's already there
    // current->todo_link == NULL means not in todo list
    if (!current->todo_link) {
        gen_workspace *ws = &gct->gens[oldest_gen->no];
        current->todo_link = ws->todo_seg;
        ws->todo_seg = current;
    }

    // Advance the current segment's next_free or allocate a new segment if full
    bool full = advance_next_free(current);
    if (full) {
        // Current segment is full: update live data estimate link it to
        // filled, take an active segment if one exists, otherwise allocate a
        // new segment.

        // Update live data estimate.
        // See Note [Live data accounting in nonmoving collector].
        unsigned int new_blocks =  nonmovingSegmentBlockCount(current) - current->next_free_snap;
        atomic_inc(&oldest_gen->live_estimate, new_blocks * nonmovingSegmentBlockSize(current) / sizeof(W_));

        // push the current segment to the filled list
        nonmovingPushFilledSegment(current);

        // first look for a new segment in the active list
        struct NonmovingSegment *new_current = pop_active_segment(alloca);

        // there are no active segments, allocate new segment
        if (new_current == NULL) {
            new_current = nonmovingAllocSegment(cap->node);
            nonmovingInitSegment(new_current, NONMOVING_ALLOCA0 + allocator_idx);
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

void nonmovingInit(void)
{
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

void nonmovingExit(void)
{
#if defined(THREADED_RTS)
    if (mark_thread) {
        debugTrace(DEBUG_nonmoving_gc,
                   "waiting for nonmoving collector thread to terminate");
        ACQUIRE_LOCK(&concurrent_coll_finished_lock);
        waitCondition(&concurrent_coll_finished, &concurrent_coll_finished_lock);
    }
    closeMutex(&concurrent_coll_finished_lock);
    closeCondition(&concurrent_coll_finished);
    closeMutex(&nonmoving_collection_mutex);
#endif
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

static void nonmovingClearBitmap(struct NonmovingSegment *seg)
{
    unsigned int n = nonmovingSegmentBlockCount(seg);
    memset(seg->bitmap, 0, n);
}

static void nonmovingClearSegmentBitmaps(struct NonmovingSegment *seg)
{
    while (seg) {
        nonmovingClearBitmap(seg);
        seg = seg->link;
    }
}

static void nonmovingClearAllBitmaps(void)
{
    for (int alloca_idx = 0; alloca_idx < NONMOVING_ALLOCA_CNT; ++alloca_idx) {
        struct NonmovingAllocator *alloca = nonmovingHeap.allocators[alloca_idx];
        nonmovingClearSegmentBitmaps(alloca->filled);
    }

    // Clear large object bits
    for (bdescr *bd = nonmoving_large_objects; bd; bd = bd->link) {
        bd->flags &= ~BF_MARKED;
    }
}

/* Prepare the heap bitmaps and snapshot metadata for a mark */
static void nonmovingPrepareMark(void)
{
    nonmovingClearAllBitmaps();
    nonmovingBumpEpoch();
    for (int alloca_idx = 0; alloca_idx < NONMOVING_ALLOCA_CNT; ++alloca_idx) {
        struct NonmovingAllocator *alloca = nonmovingHeap.allocators[alloca_idx];

        // Update current segments' snapshot pointers
        for (uint32_t cap_n = 0; cap_n < n_capabilities; ++cap_n) {
            struct NonmovingSegment *seg = alloca->current[cap_n];
            seg->next_free_snap = seg->next_free;
        }

        // Update filled segments' snapshot pointers
        struct NonmovingSegment *seg = alloca->filled;
        while (seg) {
            seg->next_free_snap = seg->next_free;
            seg = seg->link;
        }

        // N.B. It's not necessary to update snapshot pointers of active segments;
        // they were set after they were swept and haven't seen any allocation
        // since.
    }

    ASSERT(oldest_gen->scavenged_large_objects == NULL);
    bdescr *next;
    for (bdescr *bd = oldest_gen->large_objects; bd; bd = next) {
        next = bd->link;
        bd->flags |= BF_NONMOVING_SWEEPING;
        dbl_link_onto(bd, &nonmoving_large_objects);
    }
    n_nonmoving_large_blocks += oldest_gen->n_large_blocks;
    oldest_gen->large_objects = NULL;
    oldest_gen->n_large_words = 0;
    oldest_gen->n_large_blocks = 0;
    nonmoving_live_words = 0;

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

    resizeGenerations();

    nonmovingPrepareMark();
    nonmovingPrepareSweep();

    // N.B. These should have been cleared at the end of the last sweep.
    ASSERT(nonmoving_marked_large_objects == NULL);
    ASSERT(n_nonmoving_marked_large_blocks == 0);

    MarkQueue *mark_queue = stgMallocBytes(sizeof(MarkQueue), "mark queue");
    initMarkQueue(mark_queue);
    current_mark_queue = mark_queue;

    // Mark roots
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
    // a major GC, becuase that's what we do when exiting scheduler (see
    // exitScheduler()).
    if (sched_state == SCHED_RUNNING) {
        concurrent_coll_running = true;
        nonmoving_write_barrier_enabled = true;
        debugTrace(DEBUG_nonmoving_gc, "Starting concurrent mark thread");
        createOSThread(&mark_thread, "non-moving mark thread",
                       nonmovingConcurrentMark, mark_queue);
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
    nonmovingGcCafs(mark_queue);
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
    nonmovingSweepStableNameTable();

    nonmovingSweep();
    ASSERT(nonmovingHeap.sweep_list == NULL);
    debugTrace(DEBUG_nonmoving_gc, "Finished sweeping.");
    traceConcSweepEnd();
#if defined(DEBUG)
    if (RtsFlags.DebugFlags.nonmoving_gc)
        nonmovingPrintAllocatorCensus();
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

    debugBelch("Segment with %d blocks of size 2^%d (%d bytes, %lu words, scan: %p)\n",
               num_blocks,
               seg->block_size,
               1 << seg->block_size,
               ROUNDUP_BYTES_TO_WDS(1 << seg->block_size),
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
