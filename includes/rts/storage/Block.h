/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-1999
 *
 * Block structure for the storage manager
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTS_STORAGE_BLOCK_H
#define RTS_STORAGE_BLOCK_H

/* The actual block and megablock-size constants are defined in
 * includes/Constants.h, all constants here are derived from these.
 */

/* Block related constants (BLOCK_SHIFT is defined in Constants.h) */

#ifdef CMINUSMINUS
#define BLOCK_SIZE   (1<<BLOCK_SHIFT)
#else
#define BLOCK_SIZE   (1UL<<BLOCK_SHIFT)
// Note [integer overflow]
#endif

#define BLOCK_SIZE_W (BLOCK_SIZE/sizeof(W_))
#define BLOCK_MASK   (BLOCK_SIZE-1)

#define BLOCK_ROUND_UP(p)   (((W_)(p)+BLOCK_SIZE-1) & ~BLOCK_MASK)
#define BLOCK_ROUND_DOWN(p) ((void *) ((W_)(p) & ~BLOCK_MASK))

/* Megablock related constants (MBLOCK_SHIFT is defined in Constants.h) */

#ifdef CMINUSMINUS
#define MBLOCK_SIZE    (1<<MBLOCK_SHIFT)
#else
#define MBLOCK_SIZE    (1UL<<MBLOCK_SHIFT)
// Note [integer overflow]
#endif

#define MBLOCK_SIZE_W  (MBLOCK_SIZE/sizeof(W_))
#define MBLOCK_MASK    (MBLOCK_SIZE-1)

#define MBLOCK_ROUND_UP(p)   ((void *)(((W_)(p)+MBLOCK_SIZE-1) & ~MBLOCK_MASK))
#define MBLOCK_ROUND_DOWN(p) ((void *)((W_)(p) & ~MBLOCK_MASK ))

/* The largest size an object can be before we give it a block of its
 * own and treat it as an immovable object during GC, expressed as a
 * fraction of BLOCK_SIZE.
 */
#define LARGE_OBJECT_THRESHOLD ((nat)(BLOCK_SIZE * 8 / 10))

/*
 * Note [integer overflow]
 *
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

#ifndef CMINUSMINUS
typedef struct bdescr_ {

    StgPtr start;              // [READ ONLY] start addr of memory

    StgPtr free;               // first free byte of memory.
                               // NB. during use this value should lie
                               // between start and start + blocks *
                               // BLOCK_SIZE.  Values outside this
                               // range are reserved for use by the
                               // block allocator.  In particular, the
                               // value (StgPtr)(-1) is used to
                               // indicate that a block is unallocated.

    struct bdescr_ *link;      // used for chaining blocks together

    union {
        struct bdescr_ *back;  // used (occasionally) for doubly-linked lists
        StgWord *bitmap;       // bitmap for marking GC
        StgPtr  scan;          // scan pointer for copying GC
    } u;

    struct generation_ *gen;   // generation

    StgWord16 gen_no;          // gen->no, cached
    StgWord16 dest_no;         // number of destination generation
    StgWord16 _pad1;

    StgWord16 flags;           // block flags, see below

    StgWord32 blocks;          // [READ ONLY] no. of blocks in a group
                               // (if group head, 0 otherwise)

#if SIZEOF_VOID_P == 8
    StgWord32 _padding[3];
#else
    StgWord32 _padding[0];
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
/* Block is to be marked, not copied */
#define BF_MARKED    8
/* Block is free, and on the free list  (TODO: is this used?) */
#define BF_FREE      16
/* Block is executable */
#define BF_EXEC	     32
/* Block contains only a small amount of live data */
#define BF_FRAGMENTED 64
/* we know about this block (for finding leaks) */
#define BF_KNOWN     128
/* Block was swept in the last generation */
#define BF_SWEPT     256

/* Finding the block descriptor for a given block -------------------------- */

#ifdef CMINUSMINUS

#define Bdescr(p) \
    ((((p) &  MBLOCK_MASK & ~BLOCK_MASK) >> (BLOCK_SHIFT-BDESCR_SHIFT)) \
     | ((p) & ~MBLOCK_MASK))

#else

EXTERN_INLINE bdescr *Bdescr(StgPtr p);
EXTERN_INLINE bdescr *Bdescr(StgPtr p)
{
  return (bdescr *)
    ((((W_)p &  MBLOCK_MASK & ~BLOCK_MASK) >> (BLOCK_SHIFT-BDESCR_SHIFT)) 
     | ((W_)p & ~MBLOCK_MASK)
     );
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

#ifndef CMINUSMINUS // already defined in DerivedConstants.h
#define BLOCKS_PER_MBLOCK ((MBLOCK_SIZE - FIRST_BLOCK_OFF) / BLOCK_SIZE)
#endif

/* How many blocks in this megablock group */

#define MBLOCK_GROUP_BLOCKS(n) \
   (BLOCKS_PER_MBLOCK + (n-1) * (MBLOCK_SIZE / BLOCK_SIZE))

/* Compute the required size of a megablock group */

#define BLOCKS_TO_MBLOCKS(n) \
   (1 + (W_)MBLOCK_ROUND_UP((n-BLOCKS_PER_MBLOCK) * BLOCK_SIZE) / MBLOCK_SIZE)


#ifndef CMINUSMINUS 
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
bdescr *allocBlock(void);

// versions that take the storage manager lock for you:
bdescr *allocGroup_lock(W_ n);
bdescr *allocBlock_lock(void);

/* De-Allocation ----------------------------------------------------------- */

void freeGroup(bdescr *p);
void freeChain(bdescr *p);

// versions that take the storage manager lock for you:
void freeGroup_lock(bdescr *p);
void freeChain_lock(bdescr *p);

bdescr * splitBlockGroup (bdescr *bd, nat blocks);

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

INLINE_HEADER StgWord
round_up_to_mblocks(StgWord words)
{
    words += FIRST_BLOCK_OFF/sizeof(W_);
    words = ((words / MBLOCK_SIZE_W) + 1) * MBLOCK_SIZE_W;
    words -= FIRST_BLOCK_OFF/sizeof(W_);
    return words;
}

#endif /* !CMINUSMINUS */
#endif /* RTS_STORAGE_BLOCK_H */
