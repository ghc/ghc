/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2018
 *
 * Non-moving garbage collector and allocator: Mark phase
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
// We call evacuate, which expects the thread-local gc_thread to be valid;
// This is sometimes declared as a register variable therefore it is necessary
// to include the declaration so that the compiler doesn't clobber the register.
#include "NonMovingMark.h"
#include "NonMoving.h"
#include "BlockAlloc.h"  /* for countBlocks */
#include "HeapAlloc.h"
#include "Task.h"
#include "Trace.h"
#include "HeapUtils.h"
#include "Printer.h"
#include "Schedule.h"
#include "Weak.h"
#include "STM.h"
#include "MarkWeak.h"
#include "sm/Storage.h"

static void mark_closure (MarkQueue *queue, StgClosure *p, StgClosure **origin);
static void mark_tso (MarkQueue *queue, StgTSO *tso);
static void mark_stack (MarkQueue *queue, StgStack *stack);
static void mark_PAP_payload (MarkQueue *queue,
                              StgClosure *fun,
                              StgClosure **payload,
                              StgWord size);

// How many Array# entries to add to the mark queue at once?
#define MARK_ARRAY_CHUNK_LENGTH 128

/* Note [Large objects in the non-moving collector]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * The nonmoving collector keeps a separate list of its large objects, apart from
 * oldest_gen->large_objects. There are two reasons for this:
 *
 *  1. oldest_gen is mutated by minor collections, which happen concurrently with
 *     marking
 *  2. the non-moving collector needs a consistent picture
 *
 * At the beginning of a major collection, nonmovingCollect takes the objects in
 * oldest_gen->large_objects (which includes all large objects evacuated by the
 * moving collector) and adds them to nonmoving_large_objects. This is the set
 * of large objects that will being collected in the current major GC cycle.
 *
 * As the concurrent mark phase proceeds, the large objects in
 * nonmoving_large_objects that are found to be live are moved to
 * nonmoving_marked_large_objects. During sweep we discard all objects that remain
 * in nonmoving_large_objects and move everything in nonmoving_marked_larged_objects
 * back to nonmoving_large_objects.
 *
 * During minor collections large objects will accumulate on
 * oldest_gen->large_objects, where they will be picked up by the nonmoving
 * collector and moved to nonmoving_large_objects during the next major GC.
 * When this happens the block gets its BF_NONMOVING_SWEEPING flag set to
 * indicate that it is part of the snapshot and consequently should be marked by
 * the nonmoving mark phase..
 */

bdescr *nonmoving_large_objects = NULL;
bdescr *nonmoving_marked_large_objects = NULL;
memcount n_nonmoving_large_blocks = 0;
memcount n_nonmoving_marked_large_blocks = 0;
#if defined(THREADED_RTS)
/* Protects everything above. Furthermore, we only set the BF_MARKED bit of
 * large object blocks when this is held. This ensures that the write barrier
 * (e.g. finish_upd_rem_set_mark) and the collector (mark_closure) don't try to
 * move the same large object to nonmoving_marked_large_objects more than once.
 */
static Mutex nonmoving_large_objects_mutex;
#endif

/*
 * Where we keep our threads during collection since we must have a snapshot of
 * the threads that lived in the nonmoving heap at the time that the snapshot
 * was taken to safely resurrect.
 */
StgTSO *nonmoving_old_threads = END_TSO_QUEUE;
/* Same for weak pointers */
StgWeak *nonmoving_old_weak_ptr_list = NULL;
/* Because we can "tidy" thread and weak lists concurrently with a minor GC we
 * need to move marked threads and weaks to these lists until we pause for sync.
 * Then we move them to oldest_gen lists. */
StgTSO *nonmoving_threads = END_TSO_QUEUE;
StgWeak *nonmoving_weak_ptr_list = NULL;

#if defined(DEBUG)
// TODO (osa): Document
StgIndStatic *debug_caf_list_snapshot = (StgIndStatic*)END_OF_CAF_LIST;
#endif

/* Note [Update remembered set]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * The concurrent non-moving collector uses a remembered set to ensure
 * that its marking is consistent with the snapshot invariant defined in
 * the design. This remembered set, known as the update remembered set,
 * records all pointers that have been overwritten since the beginning
 * of the concurrent mark. This ensures that concurrent mutation cannot hide
 * pointers to live objects from the nonmoving garbage collector.
 *
 * The update remembered set is maintained via a write barrier that
 * is enabled whenever a concurrent mark is active. This write barrier
 * can be found in a number of places:
 *
 *  - In rts/Primops.cmm in primops responsible for modifying mutable closures
 *    (e.g. MVARs, MUT_VARs, etc.)
 *
 *  - In rts/STM.c, where
 *
 *  - In the dirty_* functions found in rts/Storage.c where we dirty MVARs,
 *    MUT_VARs, TSOs and STACKs. STACK is a somewhat special case, as described
 *    in Note [StgStack dirtiness flags and concurrent marking] in TSO.h.
 *
 *  - In the code generated by the STG code generator for pointer array writes
 *
 * There is also a read barrier to handle weak references, as described in
 * Note [Concurrent read barrier on deRefWeak#].
 *
 * The representation of the update remembered set is the same as that of
 * the mark queue. For efficiency, each capability maintains its own local
 * accumulator of remembered set entries. When a capability fills its
 * accumulator it is linked in to the global remembered set
 * (upd_rem_set_block_list), where it is consumed by the mark phase.
 *
 * The mark phase is responsible for freeing update remembered set block
 * allocations.
 *
 *
 * Note [Concurrent read barrier on deRefWeak#]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *
 * In general the non-moving GC assumes that all pointers reachable from a
 * marked object are themselves marked (or in the mark queue). However,
 * weak pointers are an obvious exception to this rule. In particular,
 * deRefWeakPtr# allows the mutator to turn a weak reference into a strong
 * reference. This interacts badly with concurrent collection. For
 * instance, consider this program:
 *
 *     f :: a -> b -> IO b
 *     f k v = do
 *         -- assume that k and v are the only references to the
 *         -- closures to which they refer.
 *         weak <- mkWeakPtr k v Nothing
 *
 *         -- N.B. k is now technically dead since the only reference to it is
 *         -- weak, but we've not yet had a chance to tombstone the WeakPtr
 *         -- (which will happen in the course of major GC).
 *         performMajorGC
 *         -- Now we are running concurrently with the mark...

 *         Just x <- deRefWeak weak
 *         -- We have now introduced a reference to `v`, which will
 *         -- not be marked as the only reference to `v` when the snapshot was
 *         -- taken is via a WeakPtr.
 *         return x
 *
 */
static Mutex upd_rem_set_lock;
bdescr *upd_rem_set_block_list = NULL;

#if defined(THREADED_RTS)
/* Used during the mark/sweep phase transition to track how many capabilities
 * have pushed their update remembered sets. Protected by upd_rem_set_lock.
 */
static volatile StgWord upd_rem_set_flush_count = 0;
#endif


/* Signaled by each capability when it has flushed its update remembered set */
static Condition upd_rem_set_flushed_cond;

/* Indicates to mutators that the write barrier must be respected. Set while
 * concurrent mark is running.
 */
StgWord nonmoving_write_barrier_enabled = false;

/* Used to provide the current mark queue to the young generation
 * collector for scavenging.
 */
MarkQueue *current_mark_queue = NULL;

/* Initialise update remembered set data structures */
void nonmovingMarkInitUpdRemSet() {
    initMutex(&upd_rem_set_lock);
    initCondition(&upd_rem_set_flushed_cond);
#if defined(THREADED_RTS)
    initMutex(&nonmoving_large_objects_mutex);
#endif
}

#if defined(THREADED_RTS) && defined(DEBUG)
static uint32_t markQueueLength(MarkQueue *q);
#endif
static void init_mark_queue_(MarkQueue *queue);

/* Transfers the given capability's update-remembered set to the global
 * remembered set.
 *
 * Really the argument type should be UpdRemSet* but this would be rather
 * inconvenient without polymorphism.
 */
void nonmovingAddUpdRemSetBlocks(MarkQueue *rset)
{
    if (markQueueIsEmpty(rset)) return;

    // find the tail of the queue
    bdescr *start = rset->blocks;
    bdescr *end = start;
    while (end->link != NULL)
        end = end->link;

    // add the blocks to the global remembered set
    ACQUIRE_LOCK(&upd_rem_set_lock);
    end->link = upd_rem_set_block_list;
    upd_rem_set_block_list = start;
    RELEASE_LOCK(&upd_rem_set_lock);

    // Reset remembered set
    ACQUIRE_SM_LOCK;
    init_mark_queue_(rset);
    rset->is_upd_rem_set = true;
    RELEASE_SM_LOCK;
}

#if defined(THREADED_RTS)
/* Called by capabilities to flush their update remembered sets when
 * synchronising with the non-moving collector as it transitions from mark to
 * sweep phase.
 */
void nonmovingFlushCapUpdRemSetBlocks(Capability *cap)
{
    debugTrace(DEBUG_nonmoving_gc,
               "Capability %d flushing update remembered set: %d",
               cap->no, markQueueLength(&cap->upd_rem_set.queue));
    nonmovingAddUpdRemSetBlocks(&cap->upd_rem_set.queue);
    atomic_inc(&upd_rem_set_flush_count, 1);
    signalCondition(&upd_rem_set_flushed_cond);
    // After this mutation will remain suspended until nonmovingFinishFlush
    // releases its capabilities.
}

/* Request that all capabilities flush their update remembered sets and suspend
 * execution until the further notice.
 */
void nonmovingBeginFlush(Task *task)
{
    debugTrace(DEBUG_nonmoving_gc, "Starting update remembered set flush...");
    upd_rem_set_flush_count = 0;
    stopAllCapabilitiesWith(NULL, task, SYNC_FLUSH_UPD_REM_SET);

    // XXX: We may have been given a capability via releaseCapability (i.e. a
    // task suspended due to a foreign call) in which case our requestSync
    // logic won't have been hit. Make sure that everyone so far has flushed.
    // Ideally we want to mark asynchronously with syncing.
    for (uint32_t i = 0; i < n_capabilities; i++) {
        nonmovingFlushCapUpdRemSetBlocks(capabilities[i]);
    }
}

/* Wait until a capability has flushed its update remembered set. Returns true
 * if all capabilities have flushed.
 */
bool nonmovingWaitForFlush()
{
    ACQUIRE_LOCK(&upd_rem_set_lock);
    debugTrace(DEBUG_nonmoving_gc, "Flush count %d", upd_rem_set_flush_count);
    bool finished = upd_rem_set_flush_count == n_capabilities;
    if (!finished) {
        waitCondition(&upd_rem_set_flushed_cond, &upd_rem_set_lock);
    }
    RELEASE_LOCK(&upd_rem_set_lock);
    return finished;
}

/* Note [Unintentional marking in resurrectThreads]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * In both moving and non-moving collectors threads found to be unreachable are
 * evacuated/marked and then resurrected with resurrectThreads. resurrectThreads
 * raises an exception in the unreachable thread via raiseAsync, which does
 * mutations on the heap. These mutations cause adding stuff to UpdRemSet of the
 * thread's capability. Here's an example backtrace where this happens:
 *
 *     #0  updateRemembSetPushClosure
 *     #1  0x000000000072b363 in dirty_TVAR
 *     #2  0x00000000007162e5 in remove_watch_queue_entries_for_trec
 *     #3  0x0000000000717098 in stmAbortTransaction
 *     #4  0x000000000070c6eb in raiseAsync
 *     #5  0x000000000070b473 in throwToSingleThreaded__
 *     #6  0x000000000070b4ab in throwToSingleThreaded
 *     #7  0x00000000006fce82 in resurrectThreads
 *     #8  0x00000000007215db in nonmovingMark_
 *     #9  0x0000000000721438 in nonmovingConcurrentMark
 *     #10 0x00007f1ee81cd6db in start_thread
 *     #11 0x00007f1ee850688f in clone
 *
 * However we don't really want to run write barriers when calling
 * resurrectThreads here, because we're in a GC pause, and overwritten values
 * are definitely gone forever (as opposed to being inserted in a marked object
 * or kept in registers and used later).
 *
 * When this happens, if we don't reset the UpdRemSets, what happens is in the
 * next mark we see these objects that were added in previous mark's
 * resurrectThreads in UpdRemSets, and mark those. This causes keeping
 * unreachable objects alive, and effects weak finalization and thread resurrect
 * (which rely on things become unreachable). As an example, stm048 fails when
 * we get this wrong, because when we do raiseAsync on a thread that was blocked
 * on an STM transaction we mutate a TVAR_WATCH_QUEUE, which has a reference to
 * the TSO that was running the STM transaction. If the TSO becomes unreachable
 * again in the next GC we don't realize this, because it was added to an
 * UpdRemSet in the previous GC's mark phase, because of raiseAsync.
 *
 * To fix this we clear all UpdRemSets in nonmovingFinishFlush, right before
 * releasing capabilities. This is somewhat inefficient (we allow adding objects
 * to UpdRemSets, only to later reset them), but the only case where we add to
 * UpdRemSets during mark is resurrectThreads, and I don't think we do so many
 * resurrection in a thread that we fill UpdRemSets and allocate new blocks. So
 * pushing an UpdRemSet in this case is really fast, and resetting is even
 * faster (we just update a pointer).
 *
 * TODO (osa): What if we actually marked UpdRemSets in this case, in the mark
 * loop? Would that work? Or what would break?
 */

/* Notify capabilities that the synchronisation is finished; they may resume
 * execution.
 */
void nonmovingFinishFlush(Task *task)
{
    // See Note [Unintentional marking in resurrectThreads]
    for (uint32_t i = 0; i < n_capabilities; i++) {
        reset_upd_rem_set(&capabilities[i]->upd_rem_set);
    }
    // Also reset upd_rem_set_block_list in case some of the UpdRemSets were
    // filled and we flushed them.
    freeChain_lock(upd_rem_set_block_list);
    upd_rem_set_block_list = NULL;

    debugTrace(DEBUG_nonmoving_gc, "Finished update remembered set flush...");
    releaseAllCapabilities(n_capabilities, NULL, task);
}
#endif

/*********************************************************
 * Pushing to either the mark queue or remembered set
 *********************************************************/

STATIC_INLINE void
push (MarkQueue *q, const MarkQueueEnt *ent)
{
    // Are we at the end of the block?
    if (q->top->head == MARK_QUEUE_BLOCK_ENTRIES) {
        // Yes, this block is full.
        if (q->is_upd_rem_set) {
            nonmovingAddUpdRemSetBlocks(q);
        } else {
            // allocate a fresh block.
            ACQUIRE_SM_LOCK;
            bdescr *bd = allocGroup(MARK_QUEUE_BLOCKS);
            bd->link = q->blocks;
            q->blocks = bd;
            q->top = (MarkQueueBlock *) bd->start;
            q->top->head = 0;
            RELEASE_SM_LOCK;
        }
    }

    q->top->entries[q->top->head] = *ent;
    q->top->head++;
}

/* A variant of push to be used by the minor GC when it encounters a reference
 * to an object in the non-moving heap. In contrast to the other push
 * operations this uses the gc_alloc_block_sync spinlock instead of the
 * SM_LOCK to allocate new blocks in the event that the mark queue is full.
 */
void
markQueuePushClosureGC (MarkQueue *q, StgClosure *p)
{
    // Are we at the end of the block?
    if (q->top->head == MARK_QUEUE_BLOCK_ENTRIES) {
        // Yes, this block is full.
        // allocate a fresh block.
        ACQUIRE_SPIN_LOCK(&gc_alloc_block_sync);
        bdescr *bd = allocGroup(MARK_QUEUE_BLOCKS);
        bd->link = q->blocks;
        q->blocks = bd;
        q->top = (MarkQueueBlock *) bd->start;
        q->top->head = 0;
        RELEASE_SPIN_LOCK(&gc_alloc_block_sync);
    }

    MarkQueueEnt ent = {
        .mark_closure = {
            .p = UNTAG_CLOSURE(p),
            .origin = NULL,
        }
    };
    q->top->entries[q->top->head] = ent;
    q->top->head++;
}

static inline
void push_closure (MarkQueue *q,
                   StgClosure *p,
                   StgClosure **origin)
{
    // TODO: Push this into callers where they already have the Bdescr
    if (HEAP_ALLOCED_GC(p) && (Bdescr((StgPtr) p)->gen != oldest_gen))
        return;

#if defined(DEBUG)
    ASSERT(LOOKS_LIKE_CLOSURE_PTR(p));
    // Commenting out: too slow
    // if (RtsFlags.DebugFlags.sanity) {
    //     assert_in_nonmoving_heap((P_)p);
    //     if (origin)
    //         assert_in_nonmoving_heap((P_)origin);
    // }
#endif

    // This must be true as origin points to a pointer and therefore must be
    // word-aligned. However, we check this as otherwise we would confuse this
    // with a mark_array entry
    ASSERT(((uintptr_t) origin & 0x3) == 0);

    MarkQueueEnt ent = {
        .mark_closure = {
            .p = UNTAG_CLOSURE(p),
            .origin = origin,
        }
    };
    push(q, &ent);
}

static
void push_array (MarkQueue *q,
                 const StgMutArrPtrs *array,
                 StgWord start_index)
{
    // TODO: Push this into callers where they already have the Bdescr
    if (HEAP_ALLOCED_GC(array) && (Bdescr((StgPtr) array)->gen != oldest_gen))
        return;

    MarkQueueEnt ent = {
        .mark_array = {
            .array = array,
            .start_index = (start_index << 16) | 0x3,
        }
    };
    push(q, &ent);
}

static
void push_thunk_srt (MarkQueue *q, const StgInfoTable *info)
{
    const StgThunkInfoTable *thunk_info = itbl_to_thunk_itbl(info);
    if (thunk_info->i.srt) {
        push_closure(q, (StgClosure*)GET_SRT(thunk_info), NULL);
    }
}

static
void push_fun_srt (MarkQueue *q, const StgInfoTable *info)
{
    const StgFunInfoTable *fun_info = itbl_to_fun_itbl(info);
    if (fun_info->i.srt) {
        push_closure(q, (StgClosure*)GET_FUN_SRT(fun_info), NULL);
    }
}

/*********************************************************
 * Pushing to the update remembered set
 *
 * upd_rem_set_push_* functions are directly called by
 * mutators and need to check whether the value is in
 * non-moving heap.
 *********************************************************/

// Check if the object is traced by the non-moving collector. This holds in two
// conditions:
//
// - Object is in non-moving heap
// - Object is a large (BF_LARGE) and marked as BF_NONMOVING
// - Object is static (HEAP_ALLOCED_GC(obj) == false)
//
static
bool check_in_nonmoving_heap(StgClosure *p) {
    if (HEAP_ALLOCED_GC(p)) {
        // This works for both large and small objects:
        return Bdescr((P_)p)->flags & BF_NONMOVING;
    } else {
        return true; // a static object
    }
}

/* Push the free variables of a (now-evaluated) thunk to the
 * update remembered set.
 */
inline void updateRemembSetPushThunk(Capability *cap, StgThunk *thunk)
{
    const StgInfoTable *info;
    do {
        info = get_volatile_itbl((StgClosure *) thunk);
    } while (info->type == WHITEHOLE);
    updateRemembSetPushThunkEager(cap, (StgThunkInfoTable *) info, thunk);
}

void updateRemembSetPushThunkEager(Capability *cap,
                                   const StgThunkInfoTable *info,
                                   StgThunk *thunk)
{
    /* N.B. info->i.type mustn't be WHITEHOLE */
    switch (info->i.type) {
    case THUNK:
    case THUNK_1_0:
    case THUNK_0_1:
    case THUNK_2_0:
    case THUNK_1_1:
    case THUNK_0_2:
    {
        MarkQueue *queue = &cap->upd_rem_set.queue;
        push_thunk_srt(queue, &info->i);

        // Don't record the origin of objects living outside of the nonmoving
        // heap; we can't perform the selector optimisation on them anyways.
        bool record_origin = check_in_nonmoving_heap((StgClosure*)thunk);

        for (StgWord i = 0; i < info->i.layout.payload.ptrs; i++) {
            if (check_in_nonmoving_heap(thunk->payload[i])) {
                push_closure(queue,
                             thunk->payload[i],
                             record_origin ? &thunk->payload[i] : NULL);
            }
        }
        break;
    }
    case AP:
    {
        MarkQueue *queue = &cap->upd_rem_set.queue;
        StgAP *ap = (StgAP *) thunk;
        push_closure(queue, ap->fun, &ap->fun);
        mark_PAP_payload(queue, ap->fun, ap->payload, ap->n_args);
        break;
    }
    case THUNK_SELECTOR:
    case BLACKHOLE:
        // TODO: This is right, right?
        break;
    default:
        barf("updateRemembSetPushThunk: invalid thunk pushed: p=%p, type=%d",
             thunk, info->i.type);
    }
}

void updateRemembSetPushThunk_(StgRegTable *reg, StgThunk *p)
{
    updateRemembSetPushThunk(regTableToCapability(reg), p);
}

inline void updateRemembSetPushClosure(Capability *cap, StgClosure *p)
{
    if (!check_in_nonmoving_heap(p)) return;
    MarkQueue *queue = &cap->upd_rem_set.queue;
    push_closure(queue, p, NULL);
}

void updateRemembSetPushClosure_(StgRegTable *reg, StgClosure *p)
{
    updateRemembSetPushClosure(regTableToCapability(reg), p);
}

STATIC_INLINE bool needs_upd_rem_set_mark(StgClosure *p)
{
    // TODO: Deduplicate with mark_closure
    bdescr *bd = Bdescr((StgPtr) p);
    if (bd->gen != oldest_gen) {
        return false;
    } else if (bd->flags & BF_LARGE) {
        if (! (bd->flags & BF_NONMOVING_SWEEPING)) {
            return false;
        } else {
            return ! (bd->flags & BF_MARKED);
        }
    } else {
        struct NonmovingSegment *seg = nonmovingGetSegment((StgPtr) p);
        nonmoving_block_idx block_idx = nonmovingGetBlockIdx((StgPtr) p);
        return nonmovingGetMark(seg, block_idx) != nonmovingMarkEpoch;
    }
}

/* Set the mark bit; only to be called *after* we have fully marked the closure */
STATIC_INLINE void finish_upd_rem_set_mark(StgClosure *p)
{
    bdescr *bd = Bdescr((StgPtr) p);
    if (bd->flags & BF_LARGE) {
        // Someone else may have already marked it.
        ACQUIRE_LOCK(&nonmoving_large_objects_mutex);
        if (! (bd->flags & BF_MARKED)) {
            bd->flags |= BF_MARKED;
            dbl_link_remove(bd, &nonmoving_large_objects);
            dbl_link_onto(bd, &nonmoving_marked_large_objects);
            n_nonmoving_large_blocks -= bd->blocks;
            n_nonmoving_marked_large_blocks += bd->blocks;
        }
        RELEASE_LOCK(&nonmoving_large_objects_mutex);
    } else {
        struct NonmovingSegment *seg = nonmovingGetSegment((StgPtr) p);
        nonmoving_block_idx block_idx = nonmovingGetBlockIdx((StgPtr) p);
        nonmovingSetMark(seg, block_idx);
    }
}

void updateRemembSetPushTSO(Capability *cap, StgTSO *tso)
{
    if (needs_upd_rem_set_mark((StgClosure *) tso)) {
        debugTrace(DEBUG_nonmoving_gc, "upd_rem_set: TSO %p", tso);
        mark_tso(&cap->upd_rem_set.queue, tso);
        finish_upd_rem_set_mark((StgClosure *) tso);
    }
}

void updateRemembSetPushStack(Capability *cap, StgStack *stack)
{
    // N.B. caller responsible for checking nonmoving_write_barrier_enabled
    if (needs_upd_rem_set_mark((StgClosure *) stack)) {
        StgWord marking = stack->marking;
        // See Note [StgStack dirtiness flags and concurrent marking]
        if (cas(&stack->marking, marking, nonmovingMarkEpoch)
              != nonmovingMarkEpoch) {
            // We have claimed the right to mark the stack.
            debugTrace(DEBUG_nonmoving_gc, "upd_rem_set: STACK %p", stack->sp);
            mark_stack(&cap->upd_rem_set.queue, stack);
            finish_upd_rem_set_mark((StgClosure *) stack);
            return;
        } else {
            // The concurrent GC has claimed the right to mark the stack.
            // Wait until it finishes marking before proceeding with
            // mutation.
            while (needs_upd_rem_set_mark((StgClosure *) stack));
#if defined(PARALLEL_GC)
                busy_wait_nop(); // TODO: Spinning here is unfortunate
#endif
            return;
        }
    }
}

/*********************************************************
 * Pushing to the mark queue
 *********************************************************/

void markQueuePush (MarkQueue *q, const MarkQueueEnt *ent)
{
    push(q, ent);
}

void markQueuePushClosure (MarkQueue *q,
                           StgClosure *p,
                           StgClosure **origin)
{
    push_closure(q, p, origin);
}

/* TODO: Do we really never want to specify the origin here? */
void markQueueAddRoot (MarkQueue* q, StgClosure** root)
{
    markQueuePushClosure(q, *root, NULL);
}

/* Push a closure to the mark queue without origin information */
void markQueuePushClosure_ (MarkQueue *q, StgClosure *p)
{
    markQueuePushClosure(q, p, NULL);
}

void markQueuePushFunSrt (MarkQueue *q, const StgInfoTable *info)
{
    push_fun_srt(q, info);
}

void markQueuePushThunkSrt (MarkQueue *q, const StgInfoTable *info)
{
    push_thunk_srt(q, info);
}

void markQueuePushArray (MarkQueue *q,
                            const StgMutArrPtrs *array,
                            StgWord start_index)
{
    push_array(q, array, start_index);
}

/*********************************************************
 * Popping from the mark queue
 *********************************************************/

// Returns invalid MarkQueueEnt if queue is empty.
static MarkQueueEnt markQueuePop_ (MarkQueue *q)
{
    MarkQueueBlock *top;

again:
    top = q->top;

    // Are we at the beginning of the block?
    if (top->head == 0) {
        // Is this the first block of the queue?
        if (q->blocks->link == NULL) {
            // Yes, therefore queue is empty...
            MarkQueueEnt none = { .null_entry = { .p = NULL } };
            return none;
        } else {
            // No, unwind to the previous block and try popping again...
            bdescr *old_block = q->blocks;
            q->blocks = old_block->link;
            q->top = (MarkQueueBlock*)q->blocks->start;
            ACQUIRE_SM_LOCK;
            freeGroup(old_block); // TODO: hold on to a block to avoid repeated allocation/deallocation?
            RELEASE_SM_LOCK;
            goto again;
        }
    }

    top->head--;
    MarkQueueEnt ent = top->entries[top->head];
    return ent;
}

static MarkQueueEnt markQueuePop (MarkQueue *q)
{
#if MARK_PREFETCH_QUEUE_DEPTH == 0
    return markQueuePop_(q);
#else
    unsigned int i = q->prefetch_head;
    while (nonmovingMarkQueueEntryType(&q->prefetch_queue[i]) == NULL_ENTRY) {
        MarkQueueEnt new = markQueuePop_(q);
        if (nonmovingMarkQueueEntryType(&new) == NULL_ENTRY) {
            // Mark queue is empty; look for any valid entries in the prefetch
            // queue
            for (unsigned int j = (i+1) % MARK_PREFETCH_QUEUE_DEPTH;
                 j != i;
                 j = (j+1) % MARK_PREFETCH_QUEUE_DEPTH)
            {
                if (nonmovingMarkQueueEntryType(&q->prefetch_queue[j]) != NULL_ENTRY) {
                    i = j;
                    goto done;
                }
            }
            return new;
        }

        // The entry may not be a MARK_CLOSURE but it doesn't matter, our
        // MarkQueueEnt encoding always places the pointer to the object to be
        // marked first.
        __builtin_prefetch(&new.mark_closure.p, 0, 0);
        q->prefetch_queue[i] = new;
        i = (i + 1) % MARK_PREFETCH_QUEUE_DEPTH;
    }

  done:
    ;
    MarkQueueEnt ret = q->prefetch_queue[i];
    q->prefetch_queue[i].null_entry.p = NULL;
    q->prefetch_head = i;
    return ret;
    //__builtin_prefetch(&prefetch_ptr[1].mark_closure.p->header.info, 0, 0);
    //__builtin_prefetch(INFO_PTR_TO_STRUCT(prefetch_ptr[3].mark_closure.p->header.info), 0, 0);
    //__builtin_prefetch(&nonmovingGetSegment_unchecked((StgPtr) prefetch_ptr[4].mark_closure.p)->block_size, 0, 0);
#endif
}

/*********************************************************
 * Creating and destroying MarkQueues and UpdRemSets
 *********************************************************/

/* Must hold sm_mutex. */
static void init_mark_queue_ (MarkQueue *queue)
{
    bdescr *bd = allocGroup(MARK_QUEUE_BLOCKS);
    queue->blocks = bd;
    queue->top = (MarkQueueBlock *) bd->start;
    queue->top->head = 0;
#if MARK_PREFETCH_QUEUE_DEPTH > 0
    memset(&queue->prefetch_queue, 0, sizeof(queue->prefetch_queue));
    queue->prefetch_head = 0;
#endif
}

/* Must hold sm_mutex. */
void initMarkQueue (MarkQueue *queue)
{
    init_mark_queue_(queue);
    queue->is_upd_rem_set = false;
}

/* Must hold sm_mutex. */
void init_upd_rem_set (UpdRemSet *rset)
{
    init_mark_queue_(&rset->queue);
    rset->queue.is_upd_rem_set = true;
}

void reset_upd_rem_set (UpdRemSet *rset)
{
    // UpdRemSets always have one block for the mark queue. This assertion is to
    // update this code if we change that.
    ASSERT(rset->queue.blocks->link == NULL);
    rset->queue.top->head = 0;
}

void freeMarkQueue (MarkQueue *queue)
{
    freeChain_lock(queue->blocks);
}

#if defined(THREADED_RTS) && defined(DEBUG)
static uint32_t
markQueueLength (MarkQueue *q)
{
    uint32_t n = 0;
    for (bdescr *block = q->blocks; block; block = block->link) {
        MarkQueueBlock *queue = (MarkQueueBlock*)block->start;
        n += queue->head;
    }
    return n;
}
#endif


/*********************************************************
 * Marking
 *********************************************************/

/*
 * N.B. Mutation of TRecHeaders is completely unprotected by any write
 * barrier. Consequently it's quite important that we deeply mark
 * any outstanding transactions.
 */
static void
mark_trec_header (MarkQueue *queue, StgTRecHeader *trec)
{
    while (trec != NO_TREC) {
        StgTRecChunk *chunk = trec->current_chunk;
        markQueuePushClosure_(queue, (StgClosure *) trec);
        markQueuePushClosure_(queue, (StgClosure *) chunk);
        while (chunk != END_STM_CHUNK_LIST) {
            for (StgWord i=0; i < chunk->next_entry_idx; i++) {
                TRecEntry *ent = &chunk->entries[i];
                markQueuePushClosure_(queue, (StgClosure *) ent->tvar);
                markQueuePushClosure_(queue, ent->expected_value);
                markQueuePushClosure_(queue, ent->new_value);
            }
            chunk = chunk->prev_chunk;
        }
        trec = trec->enclosing_trec;
    }
}

static void
mark_tso (MarkQueue *queue, StgTSO *tso)
{
    // TODO: Clear dirty if contains only old gen objects

    if (tso->bound != NULL) {
        markQueuePushClosure_(queue, (StgClosure *) tso->bound->tso);
    }

    markQueuePushClosure_(queue, (StgClosure *) tso->blocked_exceptions);
    markQueuePushClosure_(queue, (StgClosure *) tso->bq);
    mark_trec_header(queue, tso->trec);
    markQueuePushClosure_(queue, (StgClosure *) tso->stackobj);
    markQueuePushClosure_(queue, (StgClosure *) tso->_link);
    if (   tso->why_blocked == BlockedOnMVar
        || tso->why_blocked == BlockedOnMVarRead
        || tso->why_blocked == BlockedOnBlackHole
        || tso->why_blocked == BlockedOnMsgThrowTo
        || tso->why_blocked == NotBlocked
        ) {
        markQueuePushClosure_(queue, tso->block_info.closure);
    }
}

static void
do_push_closure (StgClosure **p, void *user)
{
    MarkQueue *queue = (MarkQueue *) user;
    // TODO: Origin? need reference to containing closure
    markQueuePushClosure_(queue, *p);
}

static void
mark_large_bitmap (MarkQueue *queue,
                   StgClosure **p,
                   StgLargeBitmap *large_bitmap,
                   StgWord size)
{
    walk_large_bitmap(do_push_closure, p, large_bitmap, size, queue);
}

static void
mark_small_bitmap (MarkQueue *queue, StgClosure **p, StgWord size, StgWord bitmap)
{
    while (size > 0) {
        if ((bitmap & 1) == 0) {
            // TODO: Origin?
            markQueuePushClosure(queue, *p, NULL);
        }
        p++;
        bitmap = bitmap >> 1;
        size--;
    }
}

static GNUC_ATTR_HOT
void mark_PAP_payload (MarkQueue *queue,
                       StgClosure *fun,
                       StgClosure **payload,
                       StgWord size)
{
    const StgFunInfoTable *fun_info = get_fun_itbl(UNTAG_CONST_CLOSURE(fun));
    ASSERT(fun_info->i.type != PAP);
    StgPtr p = (StgPtr) payload;

    StgWord bitmap;
    switch (fun_info->f.fun_type) {
    case ARG_GEN:
        bitmap = BITMAP_BITS(fun_info->f.b.bitmap);
        goto small_bitmap;
    case ARG_GEN_BIG:
        mark_large_bitmap(queue, payload, GET_FUN_LARGE_BITMAP(fun_info), size);
        break;
    case ARG_BCO:
        mark_large_bitmap(queue, payload, BCO_BITMAP(fun), size);
        break;
    default:
        bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
    small_bitmap:
        mark_small_bitmap(queue, (StgClosure **) p, size, bitmap);
        break;
    }
}

/* Helper for mark_stack; returns next stack frame. */
static StgPtr
mark_arg_block (MarkQueue *queue, const StgFunInfoTable *fun_info, StgClosure **args)
{
    StgWord bitmap, size;

    StgPtr p = (StgPtr)args;
    switch (fun_info->f.fun_type) {
    case ARG_GEN:
        bitmap = BITMAP_BITS(fun_info->f.b.bitmap);
        size = BITMAP_SIZE(fun_info->f.b.bitmap);
        goto small_bitmap;
    case ARG_GEN_BIG:
        size = GET_FUN_LARGE_BITMAP(fun_info)->size;
        mark_large_bitmap(queue, (StgClosure**)p, GET_FUN_LARGE_BITMAP(fun_info), size);
        p += size;
        break;
    default:
        bitmap = BITMAP_BITS(stg_arg_bitmaps[fun_info->f.fun_type]);
        size = BITMAP_SIZE(stg_arg_bitmaps[fun_info->f.fun_type]);
    small_bitmap:
        mark_small_bitmap(queue, (StgClosure**)p, size, bitmap);
        p += size;
        break;
    }
    return p;
}

static GNUC_ATTR_HOT void
mark_stack_ (MarkQueue *queue, StgPtr sp, StgPtr spBottom)
{
    ASSERT(sp <= spBottom);

    while (sp < spBottom) {
        const StgRetInfoTable *info = get_ret_itbl((StgClosure *)sp);
        switch (info->i.type) {
        case UPDATE_FRAME:
        {
            // See Note [upd-black-hole] in rts/Scav.c
            StgUpdateFrame *frame = (StgUpdateFrame *) sp;
            markQueuePushClosure_(queue, frame->updatee);
            sp += sizeofW(StgUpdateFrame);
            continue;
        }

            // small bitmap (< 32 entries, or 64 on a 64-bit machine)
        case CATCH_STM_FRAME:
        case CATCH_RETRY_FRAME:
        case ATOMICALLY_FRAME:
        case UNDERFLOW_FRAME:
        case STOP_FRAME:
        case CATCH_FRAME:
        case RET_SMALL:
        {
            StgWord bitmap = BITMAP_BITS(info->i.layout.bitmap);
            StgWord size   = BITMAP_SIZE(info->i.layout.bitmap);
            // NOTE: the payload starts immediately after the info-ptr, we
            // don't have an StgHeader in the same sense as a heap closure.
            sp++;
            mark_small_bitmap(queue, (StgClosure **) sp, size, bitmap);
            sp += size;
        }
        follow_srt:
            if (info->i.srt) {
                markQueuePushClosure_(queue, (StgClosure*)GET_SRT(info));
            }
            continue;

        case RET_BCO: {
            sp++;
            markQueuePushClosure_(queue, *(StgClosure**)sp);
            StgBCO *bco = (StgBCO *)*sp;
            sp++;
            StgWord size = BCO_BITMAP_SIZE(bco);
            mark_large_bitmap(queue, (StgClosure **) sp, BCO_BITMAP(bco), size);
            sp += size;
            continue;
        }

          // large bitmap (> 32 entries, or > 64 on a 64-bit machine)
        case RET_BIG:
        {
            StgWord size;

            size = GET_LARGE_BITMAP(&info->i)->size;
            sp++;
            mark_large_bitmap(queue, (StgClosure **) sp, GET_LARGE_BITMAP(&info->i), size);
            sp += size;
            // and don't forget to follow the SRT
            goto follow_srt;
        }

        case RET_FUN:
        {
            StgRetFun *ret_fun = (StgRetFun *)sp;
            const StgFunInfoTable *fun_info;

            markQueuePushClosure_(queue, ret_fun->fun);
            fun_info = get_fun_itbl(UNTAG_CLOSURE(ret_fun->fun));
            sp = mark_arg_block(queue, fun_info, ret_fun->payload);
            goto follow_srt;
        }

        default:
            barf("mark_stack: weird activation record found on stack: %d", (int)(info->i.type));
        }
    }
}

static GNUC_ATTR_HOT void
mark_stack (MarkQueue *queue, StgStack *stack)
{
    // TODO: Clear dirty if contains only old gen objects

    mark_stack_(queue, stack->sp, stack->stack + stack->stack_size);
}

/* See Note [Static objects under the nonmoving collector].
 *
 * Returns true if the object needs to be marked.
 */
static bool
bump_static_flag(StgClosure **link_field, StgClosure *q STG_UNUSED)
{
    while (1) {
        StgWord link = (StgWord) *link_field;
        StgWord new = (link & ~STATIC_BITS) | static_flag;
        if ((link & STATIC_BITS) == static_flag)
            return false;
        else if (cas((StgVolatilePtr) link_field, link, new) == link) {
            return true;
        }
    }
}

static GNUC_ATTR_HOT void
mark_closure (MarkQueue *queue, StgClosure *p, StgClosure **origin)
{
    (void)origin; // TODO: should be used for selector/thunk optimisations

 try_again:
    ;
    bdescr *bd = NULL;
    p = UNTAG_CLOSURE(p);

#   define PUSH_FIELD(obj, field)                                \
        markQueuePushClosure(queue,                              \
                                (StgClosure *) (obj)->field,     \
                                (StgClosure **) &(obj)->field)

    if (!HEAP_ALLOCED_GC(p)) {
        const StgInfoTable *info = get_itbl(p);
        StgHalfWord type = info->type;

        if (type == CONSTR_0_1 || type == CONSTR_0_2 || type == CONSTR_NOCAF) {
            // no need to put these on the static linked list, they don't need
            // to be marked.
            return;
        }

        switch (type) {

        case THUNK_STATIC:
            if (info->srt != 0) {
                if (bump_static_flag(THUNK_STATIC_LINK((StgClosure *)p), p)) {
                    markQueuePushThunkSrt(queue, info); // TODO this function repeats the check above
                }
            }
            return;

        case FUN_STATIC:
            if (info->srt != 0 || info->layout.payload.ptrs != 0) {
                if (bump_static_flag(STATIC_LINK(info, (StgClosure *)p), p)) {
                    markQueuePushFunSrt(queue, info); // TODO this function repeats the check above

                    // a FUN_STATIC can also be an SRT, so it may have pointer
                    // fields.  See Note [SRTs] in CmmBuildInfoTables, specifically
                    // the [FUN] optimisation.
                    // TODO (osa) I don't understand this comment
                    for (StgHalfWord i = 0; i < info->layout.payload.ptrs; ++i) {
                        PUSH_FIELD(p, payload[i]);
                    }
                }
            }
            return;

        case IND_STATIC:
            if (bump_static_flag(IND_STATIC_LINK((StgClosure *)p), p)) {
                PUSH_FIELD((StgInd *) p, indirectee);
            }
            return;

        case CONSTR:
        case CONSTR_1_0:
        case CONSTR_2_0:
        case CONSTR_1_1:
            if (bump_static_flag(STATIC_LINK(info, (StgClosure *)p), p)) {
                for (StgHalfWord i = 0; i < info->layout.payload.ptrs; ++i) {
                    PUSH_FIELD(p, payload[i]);
                }
            }
            return;

        case WHITEHOLE:
            while (get_volatile_itbl(p)->type == WHITEHOLE);
                // busy_wait_nop(); // FIXME
            goto try_again;

        default:
            barf("mark_closure(static): strange closure type %d", (int)(info->type));
        }
    }

    bd = Bdescr((StgPtr) p);

    if (bd->gen != oldest_gen) {
        // Here we have an object living outside of the non-moving heap. While
        // we likely evacuated nearly everything to the nonmoving heap during
        // preparation there are nevertheless a few ways in which we might trace
        // a reference into younger generations:
        //
        //  * a mutable object might have been updated
        //  * we might have aged an object
        return;
    }

    ASSERTM(LOOKS_LIKE_CLOSURE_PTR(p), "invalid closure, info=%p", p->header.info);

    ASSERT(!IS_FORWARDING_PTR(p->header.info));

    if (bd->flags & BF_NONMOVING) {

        if (bd->flags & BF_LARGE) {
            if (! (bd->flags & BF_NONMOVING_SWEEPING)) {
                // Not in the snapshot
                return;
            }
            if (bd->flags & BF_MARKED) {
                return;
            }

            // Mark contents
            p = (StgClosure*)bd->start;
        } else {
            struct NonmovingSegment *seg = nonmovingGetSegment((StgPtr) p);
            nonmoving_block_idx block_idx = nonmovingGetBlockIdx((StgPtr) p);

            /* We don't mark blocks that,
             *  - were not live at the time that the snapshot was taken, or
             *  - we have already marked this cycle
             */
            uint8_t mark = nonmovingGetMark(seg, block_idx);
            /* Don't mark things we've already marked (since we may loop) */
            if (mark == nonmovingMarkEpoch)
                return;

            StgClosure *snapshot_loc =
              (StgClosure *) nonmovingSegmentGetBlock(seg, seg->next_free_snap);
            if (p >= snapshot_loc && mark == 0) {
                /*
                 * In this case we are looking at a block that wasn't allocated
                 * at the time that the snapshot was taken. We mustn't trace
                 * things above the allocation pointer that aren't marked since
                 * they may not be valid objects.
                 */
                return;
            }
        }
    }

    // A pinned object that is still attached to a capability (because it's not
    // filled yet). No need to trace it pinned objects can't contain poiners.
    else if (bd->flags & BF_PINNED) {
#if defined(DEBUG)
        bool found_it = false;
        for (uint32_t i = 0; i < n_capabilities; ++i) {
            if (capabilities[i]->pinned_object_block == bd) {
                found_it = true;
                break;
            }
        }
        ASSERT(found_it);
#endif
        return;
    }

    else {
        barf("Strange closure in nonmoving mark: %p", p);
    }

    /////////////////////////////////////////////////////
    // Trace pointers
    /////////////////////////////////////////////////////

    const StgInfoTable *info = get_itbl(p);
    switch (info->type) {

    case MVAR_CLEAN:
    case MVAR_DIRTY: {
        StgMVar *mvar = (StgMVar *) p;
        PUSH_FIELD(mvar, head);
        PUSH_FIELD(mvar, tail);
        PUSH_FIELD(mvar, value);
        break;
    }

    case TVAR: {
        StgTVar *tvar = ((StgTVar *)p);
        PUSH_FIELD(tvar, current_value);
        PUSH_FIELD(tvar, first_watch_queue_entry);
        break;
    }

    case FUN_2_0:
        markQueuePushFunSrt(queue, info);
        PUSH_FIELD(p, payload[1]);
        PUSH_FIELD(p, payload[0]);
        break;

    case THUNK_2_0: {
        StgThunk *thunk = (StgThunk *) p;
        markQueuePushThunkSrt(queue, info);
        PUSH_FIELD(thunk, payload[1]);
        PUSH_FIELD(thunk, payload[0]);
        break;
    }

    case CONSTR_2_0:
        PUSH_FIELD(p, payload[1]);
        PUSH_FIELD(p, payload[0]);
        break;

    case THUNK_1_0:
        markQueuePushThunkSrt(queue, info);
        PUSH_FIELD((StgThunk *) p, payload[0]);
        break;

    case FUN_1_0:
        markQueuePushFunSrt(queue, info);
        PUSH_FIELD(p, payload[0]);
        break;

    case CONSTR_1_0:
        PUSH_FIELD(p, payload[0]);
        break;

    case THUNK_0_1:
        markQueuePushThunkSrt(queue, info);
        break;

    case FUN_0_1:
        markQueuePushFunSrt(queue, info);
        break;

    case CONSTR_0_1:
    case CONSTR_0_2:
        break;

    case THUNK_0_2:
        markQueuePushThunkSrt(queue, info);
        break;

    case FUN_0_2:
        markQueuePushFunSrt(queue, info);
        break;

    case THUNK_1_1:
        markQueuePushThunkSrt(queue, info);
        PUSH_FIELD((StgThunk *) p, payload[0]);
        break;

    case FUN_1_1:
        markQueuePushFunSrt(queue, info);
        PUSH_FIELD(p, payload[0]);
        break;

    case CONSTR_1_1:
        PUSH_FIELD(p, payload[0]);
        break;

    case FUN:
        markQueuePushFunSrt(queue, info);
        goto gen_obj;

    case THUNK: {
        markQueuePushThunkSrt(queue, info);
        for (StgWord i = 0; i < info->layout.payload.ptrs; i++) {
            StgClosure **field = &((StgThunk *) p)->payload[i];
            markQueuePushClosure(queue, *field, field);
        }
        break;
    }

    gen_obj:
    case CONSTR:
    case CONSTR_NOCAF:
    case WEAK:
    case PRIM:
    {
        for (StgWord i = 0; i < info->layout.payload.ptrs; i++) {
            StgClosure **field = &((StgClosure *) p)->payload[i];
            markQueuePushClosure(queue, *field, field);
        }
        break;
    }

    case BCO: {
        StgBCO *bco = (StgBCO *)p;
        PUSH_FIELD(bco, instrs);
        PUSH_FIELD(bco, literals);
        PUSH_FIELD(bco, ptrs);
        break;
    }


    case IND:
    case BLACKHOLE:
        PUSH_FIELD((StgInd *) p, indirectee);
        break;

    case MUT_VAR_CLEAN:
    case MUT_VAR_DIRTY:
        PUSH_FIELD((StgMutVar *)p, var);
        break;

    case BLOCKING_QUEUE: {
        StgBlockingQueue *bq = (StgBlockingQueue *)p;
        PUSH_FIELD(bq, bh);
        PUSH_FIELD(bq, owner);
        PUSH_FIELD(bq, queue);
        PUSH_FIELD(bq, link);
        break;
    }

    case THUNK_SELECTOR:
        PUSH_FIELD((StgSelector *) p, selectee);
        // TODO: selector optimization
        break;

    case AP_STACK: {
        StgAP_STACK *ap = (StgAP_STACK *)p;
        PUSH_FIELD(ap, fun);
        mark_stack_(queue, (StgPtr) ap->payload, (StgPtr) ap->payload + ap->size);
        break;
    }

    case PAP: {
        StgPAP *pap = (StgPAP *) p;
        PUSH_FIELD(pap, fun);
        mark_PAP_payload(queue, pap->fun, pap->payload, pap->n_args);
        break;
    }

    case AP: {
        StgAP *ap = (StgAP *) p;
        PUSH_FIELD(ap, fun);
        mark_PAP_payload(queue, ap->fun, ap->payload, ap->n_args);
        break;
    }

    case ARR_WORDS:
        // nothing to follow
        break;

    case MUT_ARR_PTRS_CLEAN:
    case MUT_ARR_PTRS_DIRTY:
    case MUT_ARR_PTRS_FROZEN_CLEAN:
    case MUT_ARR_PTRS_FROZEN_DIRTY:
        // TODO: Check this against Scav.c
        markQueuePushArray(queue, (StgMutArrPtrs *) p, 0);
        break;

    case SMALL_MUT_ARR_PTRS_CLEAN:
    case SMALL_MUT_ARR_PTRS_DIRTY:
    case SMALL_MUT_ARR_PTRS_FROZEN_CLEAN:
    case SMALL_MUT_ARR_PTRS_FROZEN_DIRTY: {
        StgSmallMutArrPtrs *arr = (StgSmallMutArrPtrs *) p;
        for (StgWord i = 0; i < arr->ptrs; i++) {
            StgClosure **field = &arr->payload[i];
            markQueuePushClosure(queue, *field, field);
        }
        break;
    }

    case TSO:
        mark_tso(queue, (StgTSO *) p);
        break;

    case STACK: {
        // See Note [StgStack dirtiness flags and concurrent marking]
        StgStack *stack = (StgStack *) p;
        StgWord marking = stack->marking;

        // N.B. stack->marking must be != nonmovingMarkEpoch unless
        // someone has already marked it.
        if (cas(&stack->marking, marking, nonmovingMarkEpoch)
              != nonmovingMarkEpoch) {
            // We have claimed the right to mark the stack.
            mark_stack(queue, stack);
        } else {
            // A mutator has already started marking the stack; we just let it
            // do its thing and move on. There's no reason to wait; we know that
            // the stack will be fully marked before we sweep due to the final
            // post-mark synchronization. Most importantly, we do not set its
            // mark bit, the mutator is responsible for this.
            return;
        }
        break;
    }

    case MUT_PRIM: {
        for (StgHalfWord p_idx = 0; p_idx < info->layout.payload.ptrs; ++p_idx) {
            StgClosure **field = &p->payload[p_idx];
            markQueuePushClosure(queue, *field, field);
        }
        break;
    }

    case TREC_CHUNK: {
        // TODO: Should we abort here? This should have already been marked
        // when we dirtied the TSO
        StgTRecChunk *tc = ((StgTRecChunk *) p);
        PUSH_FIELD(tc, prev_chunk);
        TRecEntry *end = &tc->entries[tc->next_entry_idx];
        for (TRecEntry *e = &tc->entries[0]; e < end; e++) {
            markQueuePushClosure_(queue, (StgClosure *) e->tvar);
            markQueuePushClosure_(queue, (StgClosure *) e->expected_value);
            markQueuePushClosure_(queue, (StgClosure *) e->new_value);
        }
        break;
    }

    case WHITEHOLE:
        while (get_volatile_itbl(p)->type == WHITEHOLE);
        goto try_again;

    default:
        barf("mark_closure: unimplemented/strange closure type %d @ %p",
             info->type, p);
    }

#   undef PUSH_FIELD

    /* Set the mark bit: it's important that we do this only after we actually push
     * the object's pointers since in the case of marking stacks there may be a
     * mutator waiting for us to finish so it can start execution.
     */
    if (bd->flags & BF_LARGE) {
        /* Marking a large object isn't idempotent since we move it to
         * nonmoving_marked_large_objects; to ensure that we don't repeatedly
         * mark a large object, we only set BF_MARKED on large objects in the
         * nonmoving heap while holding nonmoving_large_objects_mutex
         */
        ACQUIRE_LOCK(&nonmoving_large_objects_mutex);
        if (! (bd->flags & BF_MARKED)) {
            // Remove the object from nonmoving_large_objects and link it to
            // nonmoving_marked_large_objects
            dbl_link_remove(bd, &nonmoving_large_objects);
            dbl_link_onto(bd, &nonmoving_marked_large_objects);
            n_nonmoving_large_blocks -= bd->blocks;
            n_nonmoving_marked_large_blocks += bd->blocks;
            bd->flags |= BF_MARKED;
        }
        RELEASE_LOCK(&nonmoving_large_objects_mutex);
    } else {
        // TODO: Kill repetition
        struct NonmovingSegment *seg = nonmovingGetSegment((StgPtr) p);
        nonmoving_block_idx block_idx = nonmovingGetBlockIdx((StgPtr) p);
        nonmovingSetMark(seg, block_idx);
        nonmoving_live_words += nonmovingSegmentBlockSize(seg) / sizeof(W_);
    }
}

/* This is the main mark loop.
 * Invariants:
 *
 *  a. nonmovingPrepareMark has been called.
 *  b. the nursery has been fully evacuated into the non-moving generation.
 *  c. the mark queue has been seeded with a set of roots.
 *
 */
GNUC_ATTR_HOT void
nonmovingMark (MarkQueue *queue)
{
    debugTrace(DEBUG_nonmoving_gc, "Starting mark pass");
    unsigned int count = 0;
    while (true) {
        count++;
        MarkQueueEnt ent = markQueuePop(queue);

        switch (nonmovingMarkQueueEntryType(&ent)) {
        case MARK_CLOSURE:
            mark_closure(queue, ent.mark_closure.p, ent.mark_closure.origin);
            break;
        case MARK_ARRAY: {
            const StgMutArrPtrs *arr = ent.mark_array.array;
            StgWord start = ent.mark_array.start_index >> 16;
            StgWord end = start + MARK_ARRAY_CHUNK_LENGTH;
            if (end < arr->ptrs) {
                markQueuePushArray(queue, ent.mark_array.array, end);
            } else {
                end = arr->ptrs;
            }
            for (StgWord i = start; i < end; i++) {
                markQueuePushClosure_(queue, arr->payload[i]);
            }
            break;
        }
        case NULL_ENTRY:
            // Perhaps the update remembered set has more to mark...
            if (upd_rem_set_block_list) {
                ACQUIRE_LOCK(&upd_rem_set_lock);
                bdescr *old = queue->blocks;
                queue->blocks = upd_rem_set_block_list;
                queue->top = (MarkQueueBlock *) queue->blocks->start;
                upd_rem_set_block_list = NULL;
                RELEASE_LOCK(&upd_rem_set_lock);

                ACQUIRE_SM_LOCK;
                freeGroup(old);
                RELEASE_SM_LOCK;
            } else {
                // Nothing more to do
                debugTrace(DEBUG_nonmoving_gc, "Finished mark pass: %d", count);
                return;
            }
        }
    }
}

// A variant of `isAlive` that works for non-moving heap. Used for:
//
// - Collecting weak pointers; checking key of a weak pointer.
// - Resurrecting threads; checking if a thread is dead.
// - Sweeping object lists: large_objects, mut_list, stable_name_table.
//
// This may only be used after a full mark but before nonmovingSweep as it
// relies on the correctness of the next_free_snap and mark bitmaps.
bool nonmovingIsAlive (StgClosure *p)
{
    // Ignore static closures. See comments in `isAlive`.
    if (!HEAP_ALLOCED_GC(p)) {
        return true;
    }

    bdescr *bd = Bdescr((P_)p);

    // All non-static objects in the non-moving heap should be marked as
    // BF_NONMOVING
    ASSERT(bd->flags & BF_NONMOVING);

    if (bd->flags & BF_LARGE) {
        return (bd->flags & BF_NONMOVING_SWEEPING) == 0
                   // the large object wasn't in the snapshot and therefore wasn't marked
            || (bd->flags & BF_MARKED) != 0;
                   // The object was marked
    } else {
        struct NonmovingSegment *seg = nonmovingGetSegment((StgPtr) p);
        nonmoving_block_idx i = nonmovingGetBlockIdx((StgPtr) p);
        uint8_t mark =  nonmovingGetMark(seg, i);
        if (i >= seg->next_free_snap) {
            // If the object is allocated after next_free_snap then one of the
            // following must be true:
            //
            // * if its mark is 0 then the block was not allocated last time
            //   the segment was swept; however, it may have been allocated since
            //   then and therefore we must conclude that the block is alive.
            //
            // * if its mark is equal to nonmovingMarkEpoch then we found that
            //   the object was alive in the snapshot of the current GC (recall
            //   that this function may only be used after a mark).
            //   Consequently we must conclude that the object is still alive.
            //
            // * if its mark is not equal to nonmovingMarkEpoch then we found
            //   that the object was not reachable in the last snapshot.
            //   Assuming that the mark is complete we can conclude that the
            //   object is dead since the snapshot invariant guarantees that
            //   all objects alive in the snapshot would be marked.
            //
            return mark == nonmovingMarkEpoch || mark == 0;
        } else {
            // If the object is below next_free_snap then the snapshot
            // invariant guarantees that it is marked if reachable.
            return mark == nonmovingMarkEpoch;
        }
    }
}

// Check whether a snapshotted object is alive. That is for an object that we
// know to be in the snapshot, is its mark bit set. It is imperative that the
// object is in the snapshot (e.g. was in the nonmoving heap at the time that
// the snapshot was taken) since we assume that its mark bit reflects its
// reachability.
//
// This is used when
//
// - Collecting weak pointers; checking key of a weak pointer.
// - Resurrecting threads; checking if a thread is dead.
// - Sweeping object lists: large_objects, mut_list, stable_name_table.
//
static bool nonmovingIsNowAlive (StgClosure *p)
{
    // Ignore static closures. See comments in `isAlive`.
    if (!HEAP_ALLOCED_GC(p)) {
        return true;
    }

    bdescr *bd = Bdescr((P_)p);

    // All non-static objects in the non-moving heap should be marked as
    // BF_NONMOVING
    ASSERT(bd->flags & BF_NONMOVING);

    if (bd->flags & BF_LARGE) {
        return (bd->flags & BF_NONMOVING_SWEEPING) == 0
                   // the large object wasn't in the snapshot and therefore wasn't marked
            || (bd->flags & BF_MARKED) != 0;
                   // The object was marked
    } else {
        return nonmovingClosureMarkedThisCycle((P_)p);
    }
}

// Non-moving heap variant of `tidyWeakList`
bool nonmovingTidyWeaks (struct MarkQueue_ *queue)
{
    bool did_work = false;

    StgWeak **last_w = &nonmoving_old_weak_ptr_list;
    StgWeak *next_w;
    for (StgWeak *w = nonmoving_old_weak_ptr_list; w != NULL; w = next_w) {
        if (w->header.info == &stg_DEAD_WEAK_info) {
            // finalizeWeak# was called on the weak
            next_w = w->link;
            *last_w = next_w;
            continue;
        }

        // Otherwise it's a live weak
        ASSERT(w->header.info == &stg_WEAK_info);

        if (nonmovingIsNowAlive(w->key)) {
            nonmovingMarkLiveWeak(queue, w);
            did_work = true;

            // remove this weak ptr from old_weak_ptr list
            *last_w = w->link;
            next_w = w->link;

            // and put it on the weak ptr list
            w->link = nonmoving_weak_ptr_list;
            nonmoving_weak_ptr_list = w;
        } else {
            last_w = &(w->link);
            next_w = w->link;
        }
    }

    return did_work;
}

void nonmovingMarkDeadWeak (struct MarkQueue_ *queue, StgWeak *w)
{
    if (w->cfinalizers != &stg_NO_FINALIZER_closure) {
        markQueuePushClosure_(queue, w->value);
    }
    markQueuePushClosure_(queue, w->finalizer);
}

void nonmovingMarkLiveWeak (struct MarkQueue_ *queue, StgWeak *w)
{
    ASSERT(nonmovingClosureMarkedThisCycle((P_)w));
    markQueuePushClosure_(queue, w->value);
    markQueuePushClosure_(queue, w->finalizer);
    markQueuePushClosure_(queue, w->cfinalizers);
}

// When we're done with marking, any weak pointers with non-marked keys will be
// considered "dead". We mark values and finalizers of such weaks, and then
// schedule them for finalization in `scheduleFinalizers` (which we run during
// synchronization).
void nonmovingMarkDeadWeaks (struct MarkQueue_ *queue, StgWeak **dead_weaks)
{
    StgWeak *next_w;
    for (StgWeak *w = nonmoving_old_weak_ptr_list; w; w = next_w) {
        ASSERT(!nonmovingClosureMarkedThisCycle((P_)(w->key)));
        nonmovingMarkDeadWeak(queue, w);
        next_w = w ->link;
        w->link = *dead_weaks;
        *dead_weaks = w;
    }
}

// Non-moving heap variant of of `tidyThreadList`
void nonmovingTidyThreads ()
{
    StgTSO *next;
    StgTSO **prev = &nonmoving_old_threads;
    for (StgTSO *t = nonmoving_old_threads; t != END_TSO_QUEUE; t = next) {

        next = t->global_link;

        // N.B. This thread is in old_threads, consequently we *know* it is in
        // the snapshot and it is therefore safe to rely on the bitmap to
        // determine its reachability.
        if (nonmovingIsNowAlive((StgClosure*)t)) {
            // alive
            *prev = next;

            // move this thread onto threads list
            t->global_link = nonmoving_threads;
            nonmoving_threads = t;
        } else {
            // not alive (yet): leave this thread on the old_threads list
            prev = &(t->global_link);
        }
    }
}

void nonmovingResurrectThreads (struct MarkQueue_ *queue, StgTSO **resurrected_threads)
{
    StgTSO *next;
    for (StgTSO *t = nonmoving_old_threads; t != END_TSO_QUEUE; t = next) {
        next = t->global_link;

        switch (t->what_next) {
        case ThreadKilled:
        case ThreadComplete:
            continue;
        default:
            markQueuePushClosure_(queue, (StgClosure*)t);
            t->global_link = *resurrected_threads;
            *resurrected_threads = t;
        }
    }
}

#ifdef DEBUG

void printMarkQueueEntry (MarkQueueEnt *ent)
{
    switch(nonmovingMarkQueueEntryType(ent)) {
      case MARK_CLOSURE:
        debugBelch("Closure: ");
        printClosure(ent->mark_closure.p);
        break;
      case MARK_ARRAY:
        debugBelch("Array\n");
        break;
      case NULL_ENTRY:
        debugBelch("End of mark\n");
        break;
    }
}

void printMarkQueue (MarkQueue *q)
{
    debugBelch("======== MARK QUEUE ========\n");
    for (bdescr *block = q->blocks; block; block = block->link) {
        MarkQueueBlock *queue = (MarkQueueBlock*)block->start;
        for (uint32_t i = 0; i < queue->head; ++i) {
            printMarkQueueEntry(&queue->entries[i]);
        }
    }
    debugBelch("===== END OF MARK QUEUE ====\n");
}

#endif
