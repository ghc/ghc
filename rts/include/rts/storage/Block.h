/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-1999
 *
 * Block structure for the storage manager
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "ghcconfig.h"

#if !defined(CMINUSMINUS)
#include "rts/storage/HeapAlloc.h"
#endif

/* The actual block and megablock-size constants are defined in
 * rts/include/Constants.h, all constants here are derived from these.
 */

/* Block related constants (BLOCK_SHIFT is defined in Constants.h) */

#if SIZEOF_LONG == SIZEOF_VOID_P
#define UNIT 1UL
#elif SIZEOF_LONG_LONG == SIZEOF_VOID_P
#define UNIT 1ULL
#else
#error "Size of pointer is suspicious."
#endif

#if defined(CMINUSMINUS)
#define BLOCK_SIZE   (1<<BLOCK_SHIFT)
#else
#define BLOCK_SIZE   (UNIT<<BLOCK_SHIFT)
// See Note [integer overflow]
#endif

#define BLOCK_SIZE_W (BLOCK_SIZE/sizeof(W_))
#define BLOCK_MASK   (BLOCK_SIZE-1)

#define BLOCK_ROUND_UP(p)   (((W_)(p)+BLOCK_SIZE-1) & ~BLOCK_MASK)
#define BLOCK_ROUND_DOWN(p) ((void *) ((W_)(p) & ~BLOCK_MASK))

/* Megablock related constants (MBLOCK_SHIFT is defined in Constants.h) */

#if defined(CMINUSMINUS)
#define MBLOCK_SIZE    (1<<MBLOCK_SHIFT)
#else
#define MBLOCK_SIZE    (UNIT<<MBLOCK_SHIFT)
// See Note [integer overflow]
#endif

#define MBLOCK_SIZE_W  (MBLOCK_SIZE/sizeof(W_))
#define MBLOCK_MASK    (MBLOCK_SIZE-1)

#define MBLOCK_ROUND_UP(p)   ((void *)(((W_)(p)+MBLOCK_SIZE-1) & ~MBLOCK_MASK))
#define MBLOCK_ROUND_DOWN(p) ((void *)((W_)(p) & ~MBLOCK_MASK ))

/* The largest size an object can be before we give it a block of its
 * own and treat it as an immovable object during GC, expressed as a
 * fraction of BLOCK_SIZE.
 */
#define LARGE_OBJECT_THRESHOLD ((uint32_t)(BLOCK_SIZE * 8 / 10))

/*
 * Note [integer overflow]
 * ~~~~~~~~~~~~~~~~~~~~~~~
 * The UL suffix in BLOCK_SIZE and MBLOCK_SIZE promotes the expression
 * to an unsigned long, which means that expressions involving these
 * will be promoted to unsigned long, which makes integer overflow
 * less likely.  Historically, integer overflow in expressions like
 *    (n * BLOCK_SIZE)
 * where n is int or unsigned int, have caused obscure segfaults in
 * programs that use large amounts of memory (e.g. #7762, #5086).
 */

/* -----------------------------------------------------------------------------
 * Block descriptor.  This structure *must* be the right length, so we
 * can do pointer arithmetic on pointers to it.
 */

/* The block descriptor is 64 bytes on a 64-bit machine, and 32-bytes
 * on a 32-bit machine.
 */

// Note: fields marked with [READ ONLY] must not be modified by the
// client of the block allocator API.  All other fields can be
// freely modified.

#if !defined(CMINUSMINUS)


struct NonmovingSegmentInfo {
  StgWord16 allocator_idx; // nonmovingHeap.allocators[allocators_idx] is
                           // this segment's allocator.
  StgWord16 next_free_snap;
};

typedef struct bdescr_ {

    union {
        StgPtr free;               // First free byte of memory.
                                   // allocGroup() sets this to the value of start.
                                   // NB. during use this value should lie
                                   // between start and start + blocks *
                                   // BLOCK_SIZE.  Values outside this
                                   // range are reserved for use by the
                                   // block allocator.  In particular, the
                                   // value (StgPtr)(-1) is used to
                                   // indicate that a block is unallocated.
                                   //
                                   // Unused by the non-moving allocator.
        struct NonmovingSegmentInfo nonmoving_segment;
    };

    struct bdescr_ *link;      // used for chaining blocks together

    union {
        struct bdescr_ *back;  // used (occasionally) for doubly-linked lists
        StgWord *bitmap;       // bitmap for marking GC
        StgPtr  scan;          // scan pointer for copying GC
    } u;

    StgWord16 gen_no;          // gen->no, cached
    StgWord16 dest_no;         // number of destination generation
    StgWord16 node;            // which memory node does this block live on?

    StgWord16 flags;           // block flags, see below

    StgWord32 blocks;          // [READ ONLY] no. of blocks in a group
                               // (if group head, 0 otherwise)

#if SIZEOF_VOID_P == 8
    StgWord32 _padding[7];
#else
    StgWord32 _padding[2];
#endif
} bdescr;
#endif

#if SIZEOF_VOID_P == 8
#define BDESCR_SIZE  0x40
#define BDESCR_MASK  0x3f
#define BDESCR_SHIFT 6
#else
#define BDESCR_SIZE  0x20
#define BDESCR_MASK  0x1f
#define BDESCR_SHIFT 5
#endif

/* Block contains objects evacuated during this GC */
#define BF_EVACUATED 1
/* Block is a large object */
#define BF_LARGE     2
/* Block is pinned */
#define BF_PINNED    4
/* Block is to be marked, not copied. Also used for marked large objects in
 * non-moving heap. */
#define BF_MARKED    8
/* Block is executable */
#define BF_EXEC      32
/* Block contains only a small amount of live data */
#define BF_FRAGMENTED 64
/* we know about this block (for finding leaks) */
#define BF_KNOWN     128
/* Block was swept in the last generation */
#define BF_SWEPT     256
/* Block is part of a Compact */
#define BF_COMPACT   512
/* A non-moving allocator segment (see NonMoving.c) */
#define BF_NONMOVING 1024
/* A large object which has been moved to off of oldest_gen->large_objects and
 * onto nonmoving_large_objects. The mark phase ignores objects which aren't
 * so-flagged */
#define BF_NONMOVING_SWEEPING 2048
/* Maximum flag value (do not define anything higher than this!) */
#define BF_FLAG_MAX  (1 << 15)

/* Finding the block descriptor for a given block -------------------------- */

#if defined(CMINUSMINUS)

#define Bdescr(p) \
    ((((p) &  MBLOCK_MASK & ~BLOCK_MASK) >> (BLOCK_SHIFT-BDESCR_SHIFT)) \
     | ((p) & ~MBLOCK_MASK))

#define bdescr_start(bd) \
    ((((bd) & MBLOCK_MASK) << (BLOCK_SHIFT-BDESCR_SHIFT)) \
     | ((bd) & ~MBLOCK_MASK))

#else

EXTERN_INLINE bdescr *Bdescr(StgPtr p);
EXTERN_INLINE bdescr *Bdescr(StgPtr p)
{
  ASSERT(HEAP_ALLOCED_GC(p));
  return (bdescr *)
    ((((W_)p &  MBLOCK_MASK & ~BLOCK_MASK) >> (BLOCK_SHIFT-BDESCR_SHIFT))
     | ((W_)p & ~MBLOCK_MASK)
     );
}

EXTERN_INLINE StgPtr bdescr_start(const bdescr *bd);
EXTERN_INLINE StgPtr bdescr_start(const bdescr *bd)
{
    return (StgPtr)
        ((((W_)bd & MBLOCK_MASK) << (BLOCK_SHIFT-BDESCR_SHIFT))
        | ((W_)bd & ~MBLOCK_MASK)
        );
}

EXTERN_INLINE StgPtr bdescr_free(const bdescr *bd);
EXTERN_INLINE StgPtr bdescr_free(const bdescr *bd)
{
    return RELAXED_LOAD(&bd->free);
}

EXTERN_INLINE void bdescr_set_free(bdescr *bd, void *free);
EXTERN_INLINE void bdescr_set_free(bdescr *bd, void *free)
{
    RELAXED_STORE(&bd->free, free);
}

#endif

/* Useful Macros ------------------------------------------------------------ */

/* Offset of first real data block in a megablock */

#define FIRST_BLOCK_OFF \
   ((W_)BLOCK_ROUND_UP(BDESCR_SIZE * (MBLOCK_SIZE / BLOCK_SIZE)))

/* First data block in a given megablock */

#define FIRST_BLOCK(m) ((void *)(FIRST_BLOCK_OFF + (W_)(m)))

/* Last data block in a given megablock */

#define LAST_BLOCK(m)  ((void *)(MBLOCK_SIZE-BLOCK_SIZE + (W_)(m)))

/* First real block descriptor in a megablock */

#define FIRST_BDESCR(m) \
   ((bdescr *)((FIRST_BLOCK_OFF>>(BLOCK_SHIFT-BDESCR_SHIFT)) + (W_)(m)))

/* Last real block descriptor in a megablock */

#define LAST_BDESCR(m) \
  ((bdescr *)(((MBLOCK_SIZE-BLOCK_SIZE)>>(BLOCK_SHIFT-BDESCR_SHIFT)) + (W_)(m)))

/* Number of usable blocks in a megablock */

#if !defined(CMINUSMINUS) // already defined in DerivedConstants.h
#define BLOCKS_PER_MBLOCK ((MBLOCK_SIZE - FIRST_BLOCK_OFF) / BLOCK_SIZE)
#endif

/* How many blocks in this megablock group */

#define MBLOCK_GROUP_BLOCKS(n) \
   (BLOCKS_PER_MBLOCK + (n-1) * (MBLOCK_SIZE / BLOCK_SIZE))

/* Compute the required size of a megablock group */

#define BLOCKS_TO_MBLOCKS(n) \
   (1 + (W_)MBLOCK_ROUND_UP((n-BLOCKS_PER_MBLOCK) * BLOCK_SIZE) / MBLOCK_SIZE)


#if !defined(CMINUSMINUS)
/* to the end... */

/* Double-linked block lists: --------------------------------------------- */

INLINE_HEADER void
dbl_link_onto(bdescr *bd, bdescr **list)
{
  bd->link = *list;
  bd->u.back = NULL;
  if (*list) {
    (*list)->u.back = bd; /* double-link the list */
  }
  *list = bd;
}

INLINE_HEADER void
dbl_link_remove(bdescr *bd, bdescr **list)
{
    if (bd->u.back) {
        bd->u.back->link = bd->link;
    } else {
        *list = bd->link;
    }
    if (bd->link) {
        bd->link->u.back = bd->u.back;
    }
}

INLINE_HEADER void
dbl_link_insert_after(bdescr *bd, bdescr *after)
{
    bd->link = after->link;
    bd->u.back = after;
    if (after->link) {
        after->link->u.back = bd;
    }
    after->link = bd;
}

INLINE_HEADER void
dbl_link_replace(bdescr *new_, bdescr *old, bdescr **list)
{
    new_->link = old->link;
    new_->u.back = old->u.back;
    if (old->link) {
        old->link->u.back = new_;
    }
    if (old->u.back) {
        old->u.back->link = new_;
    } else {
        *list = new_;
    }
}

/* Initialisation ---------------------------------------------------------- */

extern void initBlockAllocator(void);

/* Allocation -------------------------------------------------------------- */

bdescr *allocGroup(W_ n);

EXTERN_INLINE bdescr* allocBlock(void);
EXTERN_INLINE bdescr* allocBlock(void)
{
    return allocGroup(1);
}

bdescr *allocGroupOnNode(uint32_t node, W_ n);

// Allocate n blocks, aligned at n-block boundary. The returned bdescr will
// have this invariant
//
//     bdescr_start(bdescr) % BLOCK_SIZE*n == 0
//
bdescr *allocAlignedGroupOnNode(uint32_t node, W_ n);

// Allocate a MBlock worth of `n` block sized chunks aligned at `n`-block boundry.
// This returns a linked list of `bdescr` of length `BLOCKS_PER_MBLOCK / n`.
bdescr *allocMBlockAlignedGroupOnNode(uint32_t node, W_ n);

EXTERN_INLINE bdescr* allocBlockOnNode(uint32_t node);
EXTERN_INLINE bdescr* allocBlockOnNode(uint32_t node)
{
    return allocGroupOnNode(node,1);
}

// versions that take the storage manager lock for you:
bdescr *allocGroup_lock(W_ n);
bdescr *allocBlock_lock(void);

bdescr *allocGroupOnNode_lock(uint32_t node, W_ n);
bdescr *allocBlockOnNode_lock(uint32_t node);

/* De-Allocation ----------------------------------------------------------- */

void freeGroup(bdescr *p);
void freeChain(bdescr *p);

// versions that take the storage manager lock for you:
void freeGroup_lock(bdescr *p);
void freeChain_lock(bdescr *p);

/* Round a value to megablocks --------------------------------------------- */

// We want to allocate an object around a given size, round it up or
// down to the nearest size that will fit in an mblock group.
INLINE_HEADER StgWord
round_to_mblocks(StgWord words)
{
    if (words > BLOCKS_PER_MBLOCK * BLOCK_SIZE_W) {
        // first, ignore the gap at the beginning of the first mblock by
        // adding it to the total words.  Then we can pretend we're
        // dealing in a uniform unit of megablocks.
        words += FIRST_BLOCK_OFF/sizeof(W_);

        if ((words % MBLOCK_SIZE_W) < (MBLOCK_SIZE_W / 2)) {
            words = (words / MBLOCK_SIZE_W) * MBLOCK_SIZE_W;
        } else {
            words = ((words / MBLOCK_SIZE_W) + 1) * MBLOCK_SIZE_W;
        }

        words -= FIRST_BLOCK_OFF/sizeof(W_);
    }
    return words;
}

#endif /* !CMINUSMINUS */
