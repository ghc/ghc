/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2018
 *
 * Non-moving garbage collector and allocator
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#if !defined(CMINUSMINUS)

#include <string.h>
#include "HeapAlloc.h"
#include "NonMovingMark.h"

#include "BeginPrivate.h"

// Segments
#define NONMOVING_SEGMENT_BITS 15   // 2^15 = 32kByte
// Mask to find base of segment
#define NONMOVING_SEGMENT_MASK ((1 << NONMOVING_SEGMENT_BITS) - 1)
// In bytes
#define NONMOVING_SEGMENT_SIZE (1 << NONMOVING_SEGMENT_BITS)
// In words
#define NONMOVING_SEGMENT_SIZE_W ((1 << NONMOVING_SEGMENT_BITS) / SIZEOF_VOID_P)
// In blocks
#define NONMOVING_SEGMENT_BLOCKS (NONMOVING_SEGMENT_SIZE / BLOCK_SIZE)

_Static_assert(NONMOVING_SEGMENT_SIZE % BLOCK_SIZE == 0,
               "non-moving segment size must be multiple of block size");

// The index of a block within a segment
typedef uint16_t nonmoving_block_idx;

// A non-moving heap segment
struct NonmovingSegment {
    struct NonmovingSegment *link;     // for linking together segments into lists
    struct NonmovingSegment *todo_link; // NULL when not in todo list
    nonmoving_block_idx next_free;      // index of the next unallocated block
    uint8_t bitmap[];                   // liveness bitmap
    // After the liveness bitmap comes the data blocks. Note that we need to
    // ensure that the size of this struct (including the bitmap) is a multiple
    // of the word size since GHC assumes that all object pointers are
    // so-aligned.

    // N.B. There are also bits of information which are stored in the
    // NonmovingBlockInfo stored in the segment's block descriptor. Namely:
    //
    //  * the block size can be found in nonmovingBlockInfo(seg)->log_block_size.
    //  * the next_free snapshot can be found in
    //    nonmovingBlockInfo(seg)->next_free_snap.
    //
    // This allows us to mark a nonmoving closure without bringing the
    // NonmovingSegment header into cache.
};

// This is how we mark end of todo lists. Not NULL because todo_link == NULL
// means segment is not in list.
#define END_NONMOVING_TODO_LIST ((struct NonmovingSegment*)1)

// A non-moving allocator for a particular block size
struct NonmovingAllocator {
    struct NonmovingSegment *filled;
    struct NonmovingSegment *saved_filled;
    struct NonmovingSegment *active;
    // indexed by capability number
    struct NonmovingSegment *current[];
};

// first allocator is of size 2^NONMOVING_ALLOCA0 (in bytes)
#define NONMOVING_ALLOCA0 3

// allocators cover block sizes of 2^NONMOVING_ALLOCA0 to
// 2^(NONMOVING_ALLOCA0 + NONMOVING_ALLOCA_CNT) (in bytes)
#define NONMOVING_ALLOCA_CNT 12

// maximum number of free segments to hold on to
#define NONMOVING_MAX_FREE 16

struct NonmovingHeap {
    struct NonmovingAllocator *allocators[NONMOVING_ALLOCA_CNT];
    // free segment list. This is a cache where we keep up to
    // NONMOVING_MAX_FREE segments to avoid thrashing the block allocator.
    // Note that segments in this list are still counted towards
    // oldest_gen->n_blocks.
    struct NonmovingSegment *free;
    // how many segments in free segment list? accessed atomically.
    unsigned int n_free;

    // records the current length of the nonmovingAllocator.current arrays
    unsigned int n_caps;

    // The set of segments being swept in this GC. Segments are moved here from
    // the filled list during preparation and moved back to either the filled,
    // active, or free lists during sweep.  Should be NULL before mark and
    // after sweep.
    struct NonmovingSegment *sweep_list;
};

extern struct NonmovingHeap nonmovingHeap;

extern memcount nonmoving_live_words;

#if defined(THREADED_RTS)
extern bool concurrent_coll_running;
#endif

void nonmovingInit(void);
void nonmovingStop(void);
void nonmovingExit(void);


// dead_weaks and resurrected_threads lists are used for two things:
//
// - The weaks and threads in those lists are found to be dead during
//   preparation, but the weaks will be used for finalization and threads will
//   be scheduled again (aka. resurrection) so we need to keep them alive in the
//   non-moving heap as well. So we treat them as roots and mark them.
//
// - In non-threaded runtime we add weaks and threads found to be dead in the
//   non-moving heap to those lists so that they'll be finalized and scheduled
//   as other weaks and threads. In threaded runtime we can't do this as that'd
//   cause races between a minor collection and non-moving collection. Instead
//   in non-moving heap we finalize the weaks and resurrect the threads
//   directly, but in a pause.
//
void nonmovingCollect(StgWeak **dead_weaks,
                       StgTSO **resurrected_threads);

void *nonmovingAllocate(Capability *cap, StgWord sz);
void nonmovingAddCapabilities(uint32_t new_n_caps);
void nonmovingPushFreeSegment(struct NonmovingSegment *seg);
void nonmovingClearBitmap(struct NonmovingSegment *seg);


INLINE_HEADER struct NonmovingSegmentInfo *nonmovingSegmentInfo(struct NonmovingSegment *seg) {
    return &Bdescr((StgPtr) seg)->nonmoving_segment;
}

INLINE_HEADER uint8_t nonmovingSegmentLogBlockSize(struct NonmovingSegment *seg) {
    return nonmovingSegmentInfo(seg)->log_block_size;
}

// Add a segment to the appropriate active list.
INLINE_HEADER void nonmovingPushActiveSegment(struct NonmovingSegment *seg)
{
    struct NonmovingAllocator *alloc =
        nonmovingHeap.allocators[nonmovingSegmentLogBlockSize(seg) - NONMOVING_ALLOCA0];
    while (true) {
        struct NonmovingSegment *current_active = (struct NonmovingSegment*)VOLATILE_LOAD(&alloc->active);
        seg->link = current_active;
        if (cas((StgVolatilePtr) &alloc->active, (StgWord) current_active, (StgWord) seg) == (StgWord) current_active) {
            break;
        }
    }
}

// Add a segment to the appropriate filled list.
INLINE_HEADER void nonmovingPushFilledSegment(struct NonmovingSegment *seg)
{
    struct NonmovingAllocator *alloc =
        nonmovingHeap.allocators[nonmovingSegmentLogBlockSize(seg) - NONMOVING_ALLOCA0];
    while (true) {
        struct NonmovingSegment *current_filled = (struct NonmovingSegment*)VOLATILE_LOAD(&alloc->filled);
        seg->link = current_filled;
        if (cas((StgVolatilePtr) &alloc->filled, (StgWord) current_filled, (StgWord) seg) == (StgWord) current_filled) {
            break;
        }
    }
}
// Assert that the pointer can be traced by the non-moving collector (e.g. in
// mark phase). This means one of the following:
//
// - A static object
// - A large object
// - An object in the non-moving heap (e.g. in one of the segments)
//
void assert_in_nonmoving_heap(StgPtr p);

// The block size of a given segment in bytes.
INLINE_HEADER unsigned int nonmovingSegmentBlockSize(struct NonmovingSegment *seg)
{
    return 1 << nonmovingSegmentLogBlockSize(seg);
}

// How many blocks does a segment with the given block size have?
INLINE_HEADER unsigned int nonmovingBlockCount(uint8_t log_block_size)
{
  unsigned int segment_data_size = NONMOVING_SEGMENT_SIZE - sizeof(struct NonmovingSegment);
  segment_data_size -= segment_data_size % SIZEOF_VOID_P;
  unsigned int blk_size = 1 << log_block_size;
  // N.B. +1 accounts for the byte in the mark bitmap.
  return segment_data_size / (blk_size + 1);
}

unsigned int nonmovingBlockCountFromSize(uint8_t log_block_size);

// How many blocks does the given segment contain? Also the size of the bitmap.
INLINE_HEADER unsigned int nonmovingSegmentBlockCount(struct NonmovingSegment *seg)
{
  return nonmovingBlockCountFromSize(nonmovingSegmentLogBlockSize(seg));
}

// Get a pointer to the given block index assuming that the block size is as
// given (avoiding a potential cache miss when this information is already
// available). The log_block_size argument must be equal to seg->block_size.
INLINE_HEADER void *nonmovingSegmentGetBlock_(struct NonmovingSegment *seg, uint8_t log_block_size, nonmoving_block_idx i)
{
  ASSERT(log_block_size == nonmovingSegmentLogBlockSize(seg));
  // Block size in bytes
  unsigned int blk_size = 1 << log_block_size;
  // Bitmap size in bytes
  W_ bitmap_size = nonmovingBlockCountFromSize(log_block_size) * sizeof(uint8_t);
  // Where the actual data starts (address of the first block).
  // Use ROUNDUP_BYTES_TO_WDS to align to word size. Note that
  // ROUNDUP_BYTES_TO_WDS returns in _words_, not in _bytes_, so convert it back
  // back to bytes by multiplying with word size.
  W_ data = ROUNDUP_BYTES_TO_WDS(((W_)seg) + sizeof(struct NonmovingSegment) + bitmap_size) * sizeof(W_);
  return (void*)(data + i*blk_size);
}

// Get a pointer to the given block index.
INLINE_HEADER void *nonmovingSegmentGetBlock(struct NonmovingSegment *seg, nonmoving_block_idx i)
{
  return nonmovingSegmentGetBlock_(seg, nonmovingSegmentLogBlockSize(seg), i);
}

// Get the segment which a closure resides in. Assumes that pointer points into
// non-moving heap.
INLINE_HEADER struct NonmovingSegment *nonmovingGetSegment_unchecked(StgPtr p)
{
    const uintptr_t mask = ~NONMOVING_SEGMENT_MASK;
    return (struct NonmovingSegment *) (((uintptr_t) p) & mask);
}

INLINE_HEADER struct NonmovingSegment *nonmovingGetSegment(StgPtr p)
{
    ASSERT(HEAP_ALLOCED_GC(p) && (Bdescr(p)->flags & BF_NONMOVING));
    return nonmovingGetSegment_unchecked(p);
}

INLINE_HEADER nonmoving_block_idx nonmovingGetBlockIdx(StgPtr p)
{
    ASSERT(HEAP_ALLOCED_GC(p) && (Bdescr(p)->flags & BF_NONMOVING));
    struct NonmovingSegment *seg = nonmovingGetSegment(p);
    ptrdiff_t blk0 = (ptrdiff_t)nonmovingSegmentGetBlock(seg, 0);
    ptrdiff_t offset = (ptrdiff_t)p - blk0;
    return (nonmoving_block_idx) (offset >> nonmovingSegmentLogBlockSize(seg));
}

// TODO: Eliminate this
extern uint8_t nonmovingMarkEpoch;

INLINE_HEADER void nonmovingSetMark(struct NonmovingSegment *seg, nonmoving_block_idx i)
{
    seg->bitmap[i] = nonmovingMarkEpoch;
}

INLINE_HEADER uint8_t nonmovingGetMark(struct NonmovingSegment *seg, nonmoving_block_idx i)
{
    return seg->bitmap[i];
}

INLINE_HEADER void nonmovingSetClosureMark(StgPtr p)
{
    nonmovingSetMark(nonmovingGetSegment(p), nonmovingGetBlockIdx(p));
}

// TODO: Audit the uses of these
/* Was the given closure marked this major GC cycle? */
INLINE_HEADER bool nonmovingClosureMarkedThisCycle(StgPtr p)
{
    struct NonmovingSegment *seg = nonmovingGetSegment(p);
    nonmoving_block_idx blk_idx = nonmovingGetBlockIdx(p);
    return nonmovingGetMark(seg, blk_idx) == nonmovingMarkEpoch;
}

INLINE_HEADER bool nonmovingClosureMarked(StgPtr p)
{
    struct NonmovingSegment *seg = nonmovingGetSegment(p);
    nonmoving_block_idx blk_idx = nonmovingGetBlockIdx(p);
    return nonmovingGetMark(seg, blk_idx) != 0;
}

// Can be called during a major collection to determine whether a particular
// segment is in the set of segments that will be swept this collection cycle.
INLINE_HEADER bool nonmovingSegmentBeingSwept(struct NonmovingSegment *seg)
{
    struct NonmovingSegmentInfo *seginfo = nonmovingSegmentInfo(seg);
    unsigned int n = nonmovingBlockCountFromSize(seginfo->log_block_size);
    return seginfo->next_free_snap >= n;
}

// Can be called during a major collection to determine whether a particular
// closure lives in a segment that will be swept this collection cycle.
// Note that this returns true for both large and normal objects.
INLINE_HEADER bool nonmovingClosureBeingSwept(StgClosure *p)
{
    bdescr *bd = Bdescr((StgPtr) p);
    if (HEAP_ALLOCED_GC(p)) {
        if (bd->flags & BF_NONMOVING_SWEEPING) {
            return true;
        } else if (bd->flags & BF_NONMOVING) {
            struct NonmovingSegment *seg = nonmovingGetSegment((StgPtr) p);
            return nonmovingSegmentBeingSwept(seg);
        } else {
            // outside of the nonmoving heap
            return false;
        }
    } else {
        // a static object
        return true;
    }
}

INLINE_HEADER bool isNonmovingClosure(StgClosure *p)
{
    return !HEAP_ALLOCED_GC(p) || Bdescr((P_)p)->flags & BF_NONMOVING;
}

#if defined(DEBUG)

void nonmovingPrintSegment(struct NonmovingSegment *seg);
void nonmovingPrintAllocator(struct NonmovingAllocator *alloc);
void locate_object(P_ obj);
void nonmovingPrintSweepList(void);
// Check if the object is in one of non-moving heap mut_lists
void check_in_mut_list(StgClosure *p);
void print_block_list(bdescr *bd);
void print_thread_list(StgTSO* tso);

#endif

#include "EndPrivate.h"

#endif // CMINUSMINUS
