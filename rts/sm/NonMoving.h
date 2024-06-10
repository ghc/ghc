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
#include "rts/storage/HeapAlloc.h"
#include "NonMovingMark.h"

#include "BeginPrivate.h"

// Segments
#if defined(wasm32_HOST_ARCH)
#define NONMOVING_SEGMENT_BITS 14ULL   // 2^14 = 16kByte
#else
#define NONMOVING_SEGMENT_BITS 15ULL   // 2^15 = 32kByte
#endif

// Mask to find base of segment
#define NONMOVING_SEGMENT_MASK (((uintptr_t)1 << NONMOVING_SEGMENT_BITS) - 1)
// In bytes
#define NONMOVING_SEGMENT_SIZE ((uintptr_t)1 << NONMOVING_SEGMENT_BITS)
// In words
#define NONMOVING_SEGMENT_SIZE_W (((uintptr_t)1 << NONMOVING_SEGMENT_BITS) / SIZEOF_VOID_P)
// In blocks
#define NONMOVING_SEGMENT_BLOCKS (NONMOVING_SEGMENT_SIZE / BLOCK_SIZE)

GHC_STATIC_ASSERT(NONMOVING_SEGMENT_SIZE % BLOCK_SIZE == 0, "non-moving segment size must be multiple of block size");

GHC_STATIC_ASSERT(NONMOVING_SEGMENT_BLOCKS * 2 <= BLOCKS_PER_MBLOCK, "non-moving segment size must not exceed half of mblock size");

// The index of a block within a segment
typedef uint16_t nonmoving_block_idx;

#if defined(DEBUG)
#define TRACK_SEGMENT_STATE
#endif

#if defined(TRACK_SEGMENT_STATE)
// The collector itself doesn't require each segment to know its state (this is
// implied by what segment list it is on) however it can be very useful while
// debugging to know this.
enum NonmovingSegmentState {
    FREE, CURRENT, ACTIVE, FILLED, FILLED_SWEEPING
};

#define SET_SEGMENT_STATE(seg, st) RELAXED_STORE(&(seg)->state, (st))
#define ASSERT_SEGMENT_STATE(seg, st) ASSERT(RELAXED_LOAD(&(seg)->state) == (st))
#else
#define SET_SEGMENT_STATE(_seg,_st)
#define ASSERT_SEGMENT_STATE(_seg, _st)
#endif

// A non-moving heap segment
struct NonmovingSegment {
    struct NonmovingSegment *link;      // for linking together segments into lists
    struct NonmovingSegment *todo_link; // NULL when not in todo list
    nonmoving_block_idx next_free;      // index of the next unallocated block
#if defined(TRACK_SEGMENT_STATE)
    enum NonmovingSegmentState state;
#endif
    uint8_t bitmap[];                   // liveness bitmap
    // After the liveness bitmap comes the data blocks. Note that we need to
    // ensure that the size of this struct (including the bitmap) is a multiple
    // of the word size since GHC assumes that all object pointers are
    // so-aligned.

    // N.B. There are also bits of information which are stored in the
    // NonmovingBlockInfo stored in the segment's block descriptor. Namely:
    //
    //  * the next_free snapshot can be found in
    //    nonmovingBlockInfo(seg)->next_free_snap.
    //
    // Some other information about the block size is stored on NonmovingAllocator.
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
    // N.B. Per-capabilty "current" segment lives in Capability

    // The size of each block for this allocator.
    StgWord16 block_size;
    // The amount of blocks for a segment of this allocator.
    // See nonmovingBlockCount for how this is calculated.
    StgWord16 block_count;
    // A constant for implementing the "division by a constant" optimisation.
    // Invariant:
    // (x * block_division_constant >> NONMOVING_ALLOCA_DIVIDE_SHIFT)
    // = x / block_size
    StgWord32 block_division_constant;
};

// first allocator is of size NONMOVING_ALLOCA0 (in bytes)
#define NONMOVING_ALLOCA0 8

// used in conjuction with NonmovingAllocator.block_division_constant
// to implement the "division by a constant" optimisation
#define NONMOVING_ALLOCA_DIVIDE_SHIFT 32

// amount of dense allocators.
// These cover block sizes starting with NONMOVING_ALLOCA0
// and increase in increments of NONMOVING_ALLOCA_INCREMENT
extern uint8_t nonmoving_alloca_dense_cnt;

// total amount of allocators (dense and sparse).
// allocators cover block sizes of NONMOVING_ALLOCA0 to
// NONMOVING_SEGMENT_SIZE (in bytes)
extern uint8_t nonmoving_alloca_cnt;

struct NonmovingHeap {
    struct NonmovingAllocator *allocators;
    // free segment list. This is a cache where we keep segments
    // belonging to megablocks that are only partially free.
    // Note that segments in this list are still counted towards
    // oldest_gen->n_blocks.
    struct NonmovingSegment *free;
    // saved free segment list, so the sanity checker can
    // see the segments while the free list is being pruned.
    struct NonmovingSegment *saved_free;
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

extern memcount nonmoving_segment_live_words;

void nonmovingInit(void);
void nonmovingExit(void);
bool nonmovingConcurrentMarkIsRunning(void);

bool nonmovingBlockConcurrentMark(bool wait);
void nonmovingUnblockConcurrentMark(void);

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
                      StgTSO **resurrected_threads,
                      bool concurrent);

void nonmovingPushFreeSegment(struct NonmovingSegment *seg);
void nonmovingPruneFreeSegmentList(void);

INLINE_HEADER unsigned long log2_ceil(unsigned long x)
{
    return (sizeof(unsigned long)*8) - __builtin_clzl(x-1);
}

INLINE_HEADER struct NonmovingSegmentInfo *nonmovingSegmentInfo(struct NonmovingSegment *seg) {
    return &Bdescr((StgPtr) seg)->nonmoving_segment;
}

// Find the allocator a segement belongs to
INLINE_HEADER struct NonmovingAllocator nonmovingSegmentAllocator(struct NonmovingSegment *seg) {
    return nonmovingHeap.allocators[nonmovingSegmentInfo(seg)->allocator_idx];
}

// Determine the index of the allocator for blocks of a certain size
INLINE_HEADER uint8_t nonmovingAllocatorForSize(uint16_t block_size){
   if (block_size - NONMOVING_ALLOCA0 < nonmoving_alloca_dense_cnt * (uint16_t) sizeof(StgWord)) {
       // dense case
       return (block_size  - NONMOVING_ALLOCA0) / sizeof(StgWord);
   }
   else {
       // sparse case
       return log2_ceil(block_size)
              - log2_ceil(NONMOVING_ALLOCA0 + sizeof(StgWord) * nonmoving_alloca_dense_cnt)
              + nonmoving_alloca_dense_cnt;
    }
}

// The block size of a given segment in bytes.
INLINE_HEADER unsigned int nonmovingSegmentBlockSize(struct NonmovingSegment *seg)
{
    return nonmovingSegmentAllocator(seg).block_size;
}

// Add a segment to the appropriate active list.
INLINE_HEADER void nonmovingPushActiveSegment(struct NonmovingSegment *seg)
{
    struct NonmovingAllocator *alloc = &nonmovingHeap.allocators[nonmovingAllocatorForSize(nonmovingSegmentBlockSize(seg))];
    SET_SEGMENT_STATE(seg, ACTIVE);
    while (true) {
        struct NonmovingSegment *current_active = RELAXED_LOAD(&alloc->active);
        seg->link = current_active;
        if (cas((StgVolatilePtr) &alloc->active, (StgWord) current_active, (StgWord) seg) == (StgWord) current_active) {
            break;
        }
    }
}

// Add a segment to the appropriate filled list.
INLINE_HEADER void nonmovingPushFilledSegment(struct NonmovingSegment *seg)
{
    struct NonmovingAllocator *alloc = &nonmovingHeap.allocators[nonmovingAllocatorForSize(nonmovingSegmentBlockSize(seg))];
    SET_SEGMENT_STATE(seg, FILLED);
    while (true) {
        struct NonmovingSegment *current_filled = (struct NonmovingSegment*) RELAXED_LOAD(&alloc->filled);
        RELAXED_STORE(&seg->link, current_filled);
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

// How many blocks does a segment with the given block size have?
INLINE_HEADER unsigned int nonmovingBlockCount(uint16_t block_size)
{
  unsigned int segment_data_size = NONMOVING_SEGMENT_SIZE - sizeof(struct NonmovingSegment);
  segment_data_size -= segment_data_size % SIZEOF_VOID_P;
  // N.B. +1 accounts for the byte in the mark bitmap.
  unsigned int block_count = segment_data_size / (block_size + 1);
  ASSERT(block_count < 0xfff); // must fit into StgWord16
  return block_count;
}

// How many blocks does the given segment contain? Also the size of the bitmap.
INLINE_HEADER unsigned int nonmovingSegmentBlockCount(struct NonmovingSegment *seg)
{
  return nonmovingSegmentAllocator(seg).block_count;
}

// Get a pointer to the given block index assuming that the block size is as
// given (avoiding a potential cache miss when this information is already
// available). The log_block_size argument must be equal to seg->block_size.
INLINE_HEADER void *nonmovingSegmentGetBlock_(struct NonmovingSegment *seg, uint16_t block_size, uint16_t block_count, nonmoving_block_idx i)
{
  ASSERT(block_size == nonmovingSegmentBlockSize(seg));
  // Bitmap size in bytes
  W_ bitmap_size = block_count * sizeof(uint8_t);
  // Where the actual data starts (address of the first block).
  // Use ROUNDUP_BYTES_TO_WDS to align to word size. Note that
  // ROUNDUP_BYTES_TO_WDS returns in _words_, not in _bytes_, so convert it back
  // back to bytes by multiplying with word size.
  W_ data = ROUNDUP_BYTES_TO_WDS(((W_)seg) + sizeof(struct NonmovingSegment) + bitmap_size) * sizeof(W_);
  return (void*)(data + i*block_size);
}

// Get a pointer to the given block index.
INLINE_HEADER void *nonmovingSegmentGetBlock(struct NonmovingSegment *seg, nonmoving_block_idx i)
{
  return nonmovingSegmentGetBlock_(seg, nonmovingSegmentBlockSize(seg), nonmovingSegmentBlockCount(seg), i);
}

// Get the segment which a closure resides in. Assumes that pointer points into
// non-moving heap.
INLINE_HEADER struct NonmovingSegment *nonmovingGetSegment_unchecked(StgPtr p)
{
    const uintptr_t mask = ~NONMOVING_SEGMENT_MASK;
    return (struct NonmovingSegment *) (((uintptr_t) p) & mask);
}

INLINE_HEADER bool nonmovingIsInSegment(StgPtr p)
{
    bdescr *bd = Bdescr(p);
    return HEAP_ALLOCED_GC(p) &&
        (bd->flags & BF_NONMOVING) &&
        !(bd->flags & BF_LARGE);
}

INLINE_HEADER struct NonmovingSegment *nonmovingGetSegment(StgPtr p)
{
    ASSERT(nonmovingIsInSegment(p));
    return nonmovingGetSegment_unchecked(p);
}

// Divide x by the block size of the segment.
INLINE_HEADER uint16_t nonmovingSegmentDivideBySize(struct NonmovingSegment *seg, uint16_t x)
{
    return ((StgWord64) x * nonmovingSegmentAllocator(seg).block_division_constant) >> NONMOVING_ALLOCA_DIVIDE_SHIFT;
}

INLINE_HEADER nonmoving_block_idx nonmovingGetBlockIdx(StgPtr p)
{
    struct NonmovingSegment *seg = nonmovingGetSegment(p);
    ptrdiff_t blk0 = (ptrdiff_t)nonmovingSegmentGetBlock(seg, 0);
    ptrdiff_t offset = (ptrdiff_t)p - blk0;
    return (nonmoving_block_idx) nonmovingSegmentDivideBySize(seg, offset);
}

INLINE_HEADER uint16_t nonmoving_first_sparse_allocator_size (void)
{
    return log2_ceil(NONMOVING_ALLOCA0 + (nonmoving_alloca_dense_cnt - 1) * sizeof(StgWord) + 1);
}

// TODO: Eliminate this
extern uint8_t nonmovingMarkEpoch;

INLINE_HEADER void nonmovingSetMark(struct NonmovingSegment *seg, nonmoving_block_idx i)
{
    RELAXED_STORE(&seg->bitmap[i], nonmovingMarkEpoch);
}

INLINE_HEADER uint8_t nonmovingGetMark(struct NonmovingSegment *seg, nonmoving_block_idx i)
{
    return RELAXED_LOAD(&seg->bitmap[i]);
}

INLINE_HEADER void nonmovingSetClosureMark(StgPtr p)
{
    nonmovingSetMark(nonmovingGetSegment(p), nonmovingGetBlockIdx(p));
}

INLINE_HEADER uint8_t nonmovingGetClosureMark(StgPtr p)
{
    struct NonmovingSegment *seg = nonmovingGetSegment(p);
    nonmoving_block_idx blk_idx = nonmovingGetBlockIdx(p);
    return nonmovingGetMark(seg, blk_idx);
}

/* Was the given closure marked this major GC cycle? */
INLINE_HEADER bool nonmovingClosureMarkedThisCycle(StgPtr p)
{
    return nonmovingGetClosureMark(p) == nonmovingMarkEpoch;
}

// Can be called during a major collection to determine whether a particular
// segment is in the set of segments that will be swept this collection cycle.
INLINE_HEADER bool nonmovingSegmentBeingSwept(struct NonmovingSegment *seg)
{
    struct NonmovingSegmentInfo *seginfo = nonmovingSegmentInfo(seg);
    unsigned int n = nonmovingSegmentBlockCount(seg);
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

// N.B. RtsFlags is defined as a pointer in STG code consequently this code
// doesn't typecheck.
#if !IN_STG_CODE
INLINE_HEADER bool isNonmovingClosure(StgClosure *p)
{
    return RtsFlags.GcFlags.useNonmoving && (!HEAP_ALLOCED_GC(p) || Bdescr((P_)p)->flags & BF_NONMOVING);
}
#endif

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

RTS_PRIVATE void nonmovingClearSegment(struct NonmovingSegment*);

RTS_PRIVATE void nonmovingClearSegmentFreeBlocks(struct NonmovingSegment*);

#include "EndPrivate.h"

#endif // CMINUSMINUS
