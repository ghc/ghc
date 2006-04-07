/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-1999
 *
 * Block structure for the storage manager
 *
 * ---------------------------------------------------------------------------*/

#ifndef BLOCK_H
#define BLOCK_H

/* The actual block and megablock-size constants are defined in
 * includes/Constants.h, all constants here are derived from these.
 */

/* Block related constants (BLOCK_SHIFT is defined in Constants.h) */

#define BLOCK_SIZE   (1<<BLOCK_SHIFT)
#define BLOCK_SIZE_W (BLOCK_SIZE/sizeof(W_))
#define BLOCK_MASK   (BLOCK_SIZE-1)

#define BLOCK_ROUND_UP(p)   ((void *) (((W_)(p)+BLOCK_SIZE-1) & ~BLOCK_MASK))
#define BLOCK_ROUND_DOWN(p) ((void *) ((W_)(p) & ~BLOCK_MASK))

/* Megablock related constants (MBLOCK_SHIFT is defined in Constants.h) */

#define MBLOCK_SIZE    (1<<MBLOCK_SHIFT)
#define MBLOCK_SIZE_W  (MBLOCK_SIZE/sizeof(W_))
#define MBLOCK_MASK    (MBLOCK_SIZE-1)

#define MBLOCK_ROUND_UP(p)   ((void *)(((W_)(p)+MBLOCK_SIZE-1) & ~MBLOCK_MASK))
#define MBLOCK_ROUND_DOWN(p) ((void *)((W_)(p) & ~MBLOCK_MASK ))

/* The largest size an object can be before we give it a block of its
 * own and treat it as an immovable object during GC, expressed as a
 * fraction of BLOCK_SIZE.
 */
#define LARGE_OBJECT_THRESHOLD ((nat)(BLOCK_SIZE * 8 / 10))

/* -----------------------------------------------------------------------------
 * Block descriptor.  This structure *must* be the right length, so we
 * can do pointer arithmetic on pointers to it.
 */

/* The block descriptor is 64 bytes on a 64-bit machine, and 32-bytes
 * on a 32-bit machine.
 */

#ifndef CMINUSMINUS
typedef struct bdescr_ {
  StgPtr start;			/* start addr of memory */
  StgPtr free;			/* first free byte of memory */
  struct bdescr_ *link;		/* used for chaining blocks together */
  union { 
      struct bdescr_ *back;	/* used (occasionally) for doubly-linked lists*/
      StgWord *bitmap;
  } u;
  unsigned int gen_no;		/* generation */
  struct step_ *step;		/* step */
  StgWord32 blocks;		/* no. of blocks (if grp head, 0 otherwise) */
  StgWord32 flags;              /* block is in to-space */
#if SIZEOF_VOID_P == 8
  StgWord32 _padding[2];
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
/* Block is part of a compacted generation */
#define BF_COMPACTED 8
/* Block is free, and on the free list */
#define BF_FREE      16

/* Finding the block descriptor for a given block -------------------------- */

#ifdef CMINUSMINUS

#define Bdescr(p) \
    ((((p) &  MBLOCK_MASK & ~BLOCK_MASK) >> (BLOCK_SHIFT-BDESCR_SHIFT)) \
     | ((p) & ~MBLOCK_MASK))

#else

INLINE_HEADER bdescr *Bdescr(StgPtr p)
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

/* Number of usable blocks in a megablock */

#define BLOCKS_PER_MBLOCK ((MBLOCK_SIZE - FIRST_BLOCK_OFF) / BLOCK_SIZE)

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

/* Initialisation ---------------------------------------------------------- */

extern void initBlockAllocator(void);

/* Allocation -------------------------------------------------------------- */

bdescr *allocGroup(nat n);
bdescr *allocBlock(void);

// versions that take the storage manager lock for you:
bdescr *allocGroup_lock(nat n);
bdescr *allocBlock_lock(void);

/* De-Allocation ----------------------------------------------------------- */

void freeGroup(bdescr *p);
void freeChain(bdescr *p);

// versions that take the storage manager lock for you:
void freeGroup_lock(bdescr *p);
void freeChain_lock(bdescr *p);

/* Round a value to megablocks --------------------------------------------- */

#define WORDS_PER_MBLOCK  (BLOCKS_PER_MBLOCK * BLOCK_SIZE_W)

INLINE_HEADER nat
round_to_mblocks(nat words)
{
  if (words > WORDS_PER_MBLOCK) {
    if ((words % WORDS_PER_MBLOCK) < (WORDS_PER_MBLOCK / 2)) {
      words = (words / WORDS_PER_MBLOCK) * WORDS_PER_MBLOCK;
    } else {
      words = ((words / WORDS_PER_MBLOCK) + 1) * WORDS_PER_MBLOCK;
    }
  }
  return words;
}

#endif /* !CMINUSMINUS */
#endif /* BLOCK_H */
