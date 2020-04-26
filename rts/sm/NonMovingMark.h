/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2018
 *
 * Non-moving garbage collector and allocator: Mark phase
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "Task.h"
#include "NonMoving.h"

#include "BeginPrivate.h"

enum EntryType {
    NULL_ENTRY = 0,
    MARK_CLOSURE = 1,
    MARK_ARRAY = 2
};

/* Note [Origin references in the nonmoving collector]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *
 * To implement indirection short-cutting and the selector optimisation the
 * collector needs to know where it found references, so it can update the
 * reference if it later turns out that points to an indirection. For this
 * reason, each mark queue entry contains two things:
 *
 * - a pointer to the object to be marked (p), and
 *
 * - a pointer to the field where we found the reference (origin)
 *
 * Note that the origin pointer is an interior pointer: it points not to a
 * valid closure (with info table pointer) but rather to a field inside a closure.
 * Since such references can't be safely scavenged we establish the invariant
 * that the origin pointer may only point to a field of an object living in the
 * nonmoving heap, where no scavenging is needed.
 *
 */

typedef struct {
    // Which kind of mark queue entry we have is determined by the tag bits of
    // the first word (using the tags defined by the EntryType enum).
    union {
        // A null_entry indicates the end of the queue.
        struct {
            void *p;              // must be NULL
        } null_entry;
        struct {
            StgClosure *p;        // the object to be marked
            StgClosure **origin;  // field where this reference was found.
                                  // See Note [Origin references in the nonmoving collector]
        } mark_closure;
        struct {
            const StgMutArrPtrs *array;
            StgWord start_index;  // start index is shifted to the left by 16 bits
        } mark_array;
    };
} MarkQueueEnt;

INLINE_HEADER enum EntryType nonmovingMarkQueueEntryType(MarkQueueEnt *ent)
{
    uintptr_t tag = (uintptr_t) ent->null_entry.p & TAG_MASK;
    ASSERT(tag <= MARK_ARRAY);
    return tag;
}

typedef struct {
    // index of first *unused* queue entry
    uint32_t head;

    MarkQueueEnt entries[];
} MarkQueueBlock;

// How far ahead in mark queue to prefetch?
#define MARK_PREFETCH_QUEUE_DEPTH 5

/* The mark queue is not capable of concurrent read or write.
 *
 * invariants:
 *
 *  a. top == blocks->start;
 *  b. there is always a valid MarkQueueChunk, although it may be empty
 *     (e.g. top->head == 0).
 */
typedef struct MarkQueue_ {
    // A singly link-list of blocks, each containing a MarkQueueChunk.
    bdescr *blocks;

    // Cached value of blocks->start.
    MarkQueueBlock *top;

    // Is this a mark queue or a capability-local update remembered set?
    bool is_upd_rem_set;

#if MARK_PREFETCH_QUEUE_DEPTH > 0
    // A ring-buffer of entries which we will mark next
    MarkQueueEnt prefetch_queue[MARK_PREFETCH_QUEUE_DEPTH];
    // The first free slot in prefetch_queue.
    uint8_t prefetch_head;
#endif
} MarkQueue;

/* While it shares its representation with MarkQueue, UpdRemSet differs in
 * behavior when pushing; namely full chunks are immediately pushed to the
 * global update remembered set, not accumulated into a chain. We make this
 * distinction apparent in the types.
 */
typedef struct {
    MarkQueue queue;
} UpdRemSet;

// Number of blocks to allocate for a mark queue
#define MARK_QUEUE_BLOCKS 16

// The length of MarkQueueBlock.entries
#define MARK_QUEUE_BLOCK_ENTRIES ((MARK_QUEUE_BLOCKS * BLOCK_SIZE - sizeof(MarkQueueBlock)) / sizeof(MarkQueueEnt))

extern bdescr *nonmoving_large_objects, *nonmoving_marked_large_objects,
              *nonmoving_compact_objects, *nonmoving_marked_compact_objects;
extern memcount n_nonmoving_large_blocks, n_nonmoving_marked_large_blocks,
                n_nonmoving_compact_blocks, n_nonmoving_marked_compact_blocks;

extern StgTSO *nonmoving_old_threads;
extern StgWeak *nonmoving_old_weak_ptr_list;
extern StgTSO *nonmoving_threads;
extern StgWeak *nonmoving_weak_ptr_list;

#if defined(DEBUG)
extern StgIndStatic *debug_caf_list_snapshot;
#endif

extern MarkQueue *current_mark_queue;
extern bdescr *upd_rem_set_block_list;


void nonmovingMarkInitUpdRemSet(void);

void init_upd_rem_set(UpdRemSet *rset);
void reset_upd_rem_set(UpdRemSet *rset);
void updateRemembSetPushClosure(Capability *cap, StgClosure *p);
void updateRemembSetPushThunk(Capability *cap, StgThunk *p);
void updateRemembSetPushTSO(Capability *cap, StgTSO *tso);
void updateRemembSetPushStack(Capability *cap, StgStack *stack);

#if defined(THREADED_RTS)
void nonmovingFlushCapUpdRemSetBlocks(Capability *cap);
void nonmovingBeginFlush(Task *task);
bool nonmovingWaitForFlush(void);
void nonmovingFinishFlush(Task *task);
#endif

void markQueueAddRoot(MarkQueue* q, StgClosure** root);

void initMarkQueue(MarkQueue *queue);
void freeMarkQueue(MarkQueue *queue);
void nonmovingMark(struct MarkQueue_ *restrict queue);

bool nonmovingTidyWeaks(struct MarkQueue_ *queue);
void nonmovingTidyThreads(void);
void nonmovingMarkDeadWeaks(struct MarkQueue_ *queue, StgWeak **dead_weak_ptr_list);
void nonmovingResurrectThreads(struct MarkQueue_ *queue, StgTSO **resurrected_threads);
bool nonmovingIsAlive(StgClosure *p);
void nonmovingMarkDeadWeak(struct MarkQueue_ *queue, StgWeak *w);
void nonmovingMarkLiveWeak(struct MarkQueue_ *queue, StgWeak *w);
void nonmovingAddUpdRemSetBlocks(struct MarkQueue_ *rset);

void markQueuePush(MarkQueue *q, const MarkQueueEnt *ent);
void markQueuePushClosureGC(MarkQueue *q, StgClosure *p);
void markQueuePushClosure(MarkQueue *q,
                             StgClosure *p,
                             StgClosure **origin);
void markQueuePushClosure_(MarkQueue *q, StgClosure *p);
void markQueuePushThunkSrt(MarkQueue *q, const StgInfoTable *info);
void markQueuePushFunSrt(MarkQueue *q, const StgInfoTable *info);
void markQueuePushArray(MarkQueue *q, const StgMutArrPtrs *array, StgWord start_index);
void updateRemembSetPushThunkEager(Capability *cap,
                                  const StgThunkInfoTable *orig_info,
                                  StgThunk *thunk);

INLINE_HEADER bool markQueueIsEmpty(MarkQueue *q)
{
    return (q->blocks == NULL) || (q->top->head == 0 && q->blocks->link == NULL);
}

#if defined(DEBUG)

void printMarkQueueEntry(MarkQueueEnt *ent);
void printMarkQueue(MarkQueue *q);

#endif

#include "EndPrivate.h"
