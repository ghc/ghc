/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2014
 *
 * GC support for immutable non-GCed structures, also known as Compact
 * Normal Forms (CNF for short). This provides the RTS support for
 * the 'compact' package and the Data.Compact module.
 *
 * ---------------------------------------------------------------------------*/

#define _GNU_SOURCE

#include "PosixSource.h"
#include <string.h>
#include "Rts.h"
#include "RtsUtils.h"

#include "Capability.h"
#include "GC.h"
#include "Storage.h"
#include "CNF.h"
#include "Hash.h"
#include "HeapAlloc.h"
#include "BlockAlloc.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif

/**
 * Note [Compact Normal Forms]
 *
 * A Compact Normal Form, is at its essence a chain of memory blocks (multiple
 * of block allocator blocks) containing other closures inside.
 *
 * Each block starts with a header, of type StgCompactNFDataBlock, that points
 * to the first and to the next block in the chain. Right after the header
 * in the first block we have a closure of type StgCompactNFData, which holds
 * compact-wide metadata. This closure is the Compact# that Cmm and Haskell
 * see, and it's mostly a regular Haskell closure.
 *
 * Blocks are appended to the chain automatically as needed, or manually with a
 * compactResize() call, which also adjust the size of automatically appended
 * blocks.
 *
 * Objects can be appended to the block currently marked to the nursery, or any
 * of the later blocks if the nursery block is too full to fit the entire
 * object. For each block in the chain (which can be multiple block allocator
 * blocks), we use the bdescr of its beginning to store how full it is.
 * After an object is appended, it is scavenged for any outgoing pointers,
 * and all pointed to objects are appended, recursively, in a manner similar
 * to copying GC (further discussion in the note [Appending to a Compact])
 *
 * We also flag each bdescr in each block allocator block of a compact
 * (including those there were obtained as second or later from a single
 * allocGroup(n) call) with the BF_COMPACT. This allows the GC to quickly
 * realize that a given pointer is in a compact region, and trigger the
 * CNF path.
 *
 * These two facts combined mean that in any compact block where some object
 * begins bdescrs must be valid. For this simplicity this is achieved by
 * restricting the maximum size of a compact block to 252 block allocator
 * blocks (so that the total with the bdescr is one megablock).
 *
 * Compacts as a whole live in special list in each generation, where the
 * list is held through the bd->link field of the bdescr of the StgCompactNFData
 * closure (as for large objects). They live in a different list than large
 * objects because the operation to free them is different (all blocks in
 * a compact must be freed individually), and stats/sanity behavior are
 * slightly different. This is also the reason that compact allocates memory
 * using a special function instead of just calling allocate().
 *
 * Compacts are also suitable for network or disk serialization, and to
 * that extent they support a pointer fixup operation, which adjusts pointers
 * from a previous layout of the chain in memory to the new allocation.
 * This works by constructing a temporary binary search table (in the C heap)
 * of the old block addresses (which are known from the block header), and
 * then searching for each pointer in the table, and adjusting it.
 * It relies on ABI compatibility and static linking (or no ASLR) because it
 * does not attempt to reconstruct info tables, and uses info tables to detect
 * pointers. In practice this means only the exact same binary should be
 * used.
 */

typedef enum {
    ALLOCATE_APPEND,
    ALLOCATE_NEW,
    ALLOCATE_IMPORT_NEW,
    ALLOCATE_IMPORT_APPEND,
} AllocateOp;

static StgCompactNFDataBlock *
compactAllocateBlockInternal(Capability            *cap,
                             StgWord                aligned_size,
                             StgCompactNFDataBlock *first,
                             AllocateOp             operation)
{
    StgCompactNFDataBlock *self;
    bdescr *block, *head;
    uint32_t n_blocks;
    generation *g;

    n_blocks = aligned_size / BLOCK_SIZE;

    // Attempting to allocate an object larger than maxHeapSize
    // should definitely be disallowed.  (bug #1791)
    if ((RtsFlags.GcFlags.maxHeapSize > 0 &&
         n_blocks >= RtsFlags.GcFlags.maxHeapSize) ||
        n_blocks >= HS_INT32_MAX)   // avoid overflow when
                                    // calling allocGroup() below
    {
        heapOverflow();
        // heapOverflow() doesn't exit (see #2592), but we aren't
        // in a position to do a clean shutdown here: we
        // either have to allocate the memory or exit now.
        // Allocating the memory would be bad, because the user
        // has requested that we not exceed maxHeapSize, so we
        // just exit.
        stg_exit(EXIT_HEAPOVERFLOW);
    }

    // It is imperative that first is the first block in the compact
    // (or NULL if the compact does not exist yet)
    // because the evacuate code does not update the generation of
    // blocks other than the first (so we would get the statistics
    // wrong and crash in Sanity)
    if (first != NULL) {
        block = Bdescr((P_)first);
        g = block->gen;
    } else {
        g = g0;
    }

    ACQUIRE_SM_LOCK;
    block = allocGroup(n_blocks);
    switch (operation) {
    case ALLOCATE_NEW:
        ASSERT (first == NULL);
        ASSERT (g == g0);
        dbl_link_onto(block, &g0->compact_objects);
        g->n_compact_blocks += block->blocks;
        g->n_new_large_words += aligned_size / sizeof(StgWord);
        break;

    case ALLOCATE_IMPORT_NEW:
        dbl_link_onto(block, &g0->compact_blocks_in_import);
        /* fallthrough */
    case ALLOCATE_IMPORT_APPEND:
        ASSERT (first == NULL);
        ASSERT (g == g0);
        g->n_compact_blocks_in_import += block->blocks;
        g->n_new_large_words += aligned_size / sizeof(StgWord);
        break;

    case ALLOCATE_APPEND:
        g->n_compact_blocks += block->blocks;
        if (g == g0)
            g->n_new_large_words += aligned_size / sizeof(StgWord);
        break;

    default:
#ifdef DEBUG
        ASSERT(!"code should not be reached");
#else
        RTS_UNREACHABLE;
#endif
    }
    RELEASE_SM_LOCK;

    cap->total_allocated += aligned_size / sizeof(StgWord);

    self = (StgCompactNFDataBlock*) block->start;
    self->self = self;
    self->next = NULL;

    head = block;
    initBdescr(head, g, g);
    head->flags = BF_COMPACT;
    for (block = head + 1, n_blocks --; n_blocks > 0; block++, n_blocks--) {
        block->link = head;
        block->blocks = 0;
        block->flags = BF_COMPACT;
    }

    return self;
}

static inline StgCompactNFDataBlock *
compactGetFirstBlock(StgCompactNFData *str)
{
    return (StgCompactNFDataBlock*) ((W_)str - sizeof(StgCompactNFDataBlock));
}

static inline StgCompactNFData *
firstBlockGetCompact(StgCompactNFDataBlock *block)
{
    return (StgCompactNFData*) ((W_)block + sizeof(StgCompactNFDataBlock));
}

static void
freeBlockChain(StgCompactNFDataBlock *block)
{
    StgCompactNFDataBlock *next;
    bdescr *bd;

    for ( ; block; block = next) {
        next = block->next;
        bd = Bdescr((StgPtr)block);
        ASSERT((bd->flags & BF_EVACUATED) == 0);
        freeGroup(bd);
    }
}

void
compactFree(StgCompactNFData *str)
{
    StgCompactNFDataBlock *block;

    block = compactGetFirstBlock(str);
    freeBlockChain(block);
}

void
compactMarkKnown(StgCompactNFData *str)
{
    bdescr *bd;
    StgCompactNFDataBlock *block;

    block = compactGetFirstBlock(str);
    for ( ; block; block = block->next) {
        bd = Bdescr((StgPtr)block);
        bd->flags |= BF_KNOWN;
    }
}

StgWord
countCompactBlocks(bdescr *outer)
{
    StgCompactNFDataBlock *block;
    W_ count;

    count = 0;
    while (outer) {
        bdescr *inner;

        block = (StgCompactNFDataBlock*)(outer->start);
        do {
            inner = Bdescr((P_)block);
            ASSERT (inner->flags & BF_COMPACT);

            count += inner->blocks;
            block = block->next;
        } while(block);

        outer = outer->link;
    }

    return count;
}

StgCompactNFData *
compactNew (Capability *cap, StgWord size)
{
    StgWord aligned_size;
    StgCompactNFDataBlock *block;
    StgCompactNFData *self;
    bdescr *bd;

    aligned_size = BLOCK_ROUND_UP(size + sizeof(StgCompactNFDataBlock)
                                  + sizeof(StgCompactNFDataBlock));
    if (aligned_size >= BLOCK_SIZE * BLOCKS_PER_MBLOCK)
        aligned_size = BLOCK_SIZE * BLOCKS_PER_MBLOCK;

    block = compactAllocateBlockInternal(cap, aligned_size, NULL,
                                         ALLOCATE_NEW);

    self = firstBlockGetCompact(block);
    SET_INFO((StgClosure*)self, &stg_COMPACT_NFDATA_info);
    self->totalDataW = aligned_size / sizeof(StgWord);
    self->autoBlockW = aligned_size / sizeof(StgWord);
    self->nursery = block;
    self->last = block;

    block->owner = self;

    bd = Bdescr((P_)block);
    bd->free = (StgPtr)((W_)self + sizeof(StgCompactNFData));
    ASSERT (bd->free == (StgPtr)self + sizeofW(StgCompactNFData));

    self->totalW = bd->blocks * BLOCK_SIZE_W;

    return self;
}

static StgCompactNFDataBlock *
compactAppendBlock (Capability       *cap,
                    StgCompactNFData *str,
                    StgWord           aligned_size)
{
    StgCompactNFDataBlock *block;
    bdescr *bd;

    block = compactAllocateBlockInternal(cap, aligned_size,
                                         compactGetFirstBlock(str),
                                         ALLOCATE_APPEND);
    block->owner = str;
    block->next = NULL;

    ASSERT (str->last->next == NULL);
    str->last->next = block;
    str->last = block;
    if (str->nursery == NULL)
        str->nursery = block;
    str->totalDataW += aligned_size / sizeof(StgWord);

    bd = Bdescr((P_)block);
    bd->free = (StgPtr)((W_)block + sizeof(StgCompactNFDataBlock));
    ASSERT (bd->free == (StgPtr)block + sizeofW(StgCompactNFDataBlock));

    str->totalW += bd->blocks * BLOCK_SIZE_W;

    return block;
}

void
compactResize (Capability *cap, StgCompactNFData *str, StgWord new_size)
{
    StgWord aligned_size;

    aligned_size = BLOCK_ROUND_UP(new_size + sizeof(StgCompactNFDataBlock));
    if (aligned_size >= BLOCK_SIZE * BLOCKS_PER_MBLOCK)
        aligned_size = BLOCK_SIZE * BLOCKS_PER_MBLOCK;

    str->autoBlockW = aligned_size / sizeof(StgWord);

    compactAppendBlock(cap, str, aligned_size);
}

/* Note [Appending to a Compact]

   This is a simple reimplementation of the copying GC.
   One could be tempted to reuse the actual GC code here, but he
   would quickly find out that it would bring all the generational
   GC complexity for no need at all.

   Plus, we don't need to scavenge/evacuate all kinds of weird
   objects here, just constructors and primitives. Thunks are
   expected to be evaluated before appending by the API layer
   (in Haskell, above the primop which is implemented here).
   Also, we have a different policy for large objects: instead
   of relinking to the new large object list, we fully copy
   them inside the compact and scavenge them normally.

   Note that if we allowed thunks and lazy evaluation the compact
   would be a mutable object, which would create all sorts of
   GC problems (besides, evaluating a thunk could exaust the
   compact space or yield an invalid object, and we would have
   no way to signal that to the user)

   Just like the real evacuate/scavenge pairs, we need to handle
   object loops. We would want to use the same strategy of rewriting objects
   with forwarding pointer, but in a real GC, at the end the
   blocks from the old space are dropped (dropping all forwarding
   pointers at the same time), which we can't do here as we don't
   know all pointers to the objects being evacuated. Also, in parallel
   we don't know which other threads are evaluating the thunks
   that we just corrupted at the same time.

   So instead we use a hash table of "visited" objects, and add
   the pointer as we copy it. To reduce the overhead, we also offer
   a version of the API that does not preserve sharing (TODO).

   You might be tempted to replace the objects with StdInd to
   the object in the compact, but you would be wrong: the haskell
   code assumes that objects in the heap only become more evaluated
   (thunks to blackholes to inds to actual objects), and in
   particular it assumes that if a pointer is tagged the object
   is directly referenced and the values can be read directly,
   without entering the closure.

   FIXME: any better idea than the hash table?
*/

static void
unroll_memcpy(StgPtr to, StgPtr from, StgWord size)
{
    for (; size > 0; size--)
        *(to++) = *(from++);
}

static rtsBool
allocate_in_compact (StgCompactNFDataBlock *block, StgWord sizeW, StgPtr *at)
{
    bdescr *bd;
    StgPtr top;
    StgPtr free;

    bd = Bdescr((StgPtr)block);
    top = bd->start + BLOCK_SIZE_W * bd->blocks;
    if (bd->free + sizeW > top)
        return rtsFalse;

    free = bd->free;
    bd->free += sizeW;
    *at = free;

    return rtsTrue;
}

static rtsBool
block_is_full (StgCompactNFDataBlock *block)
{
    bdescr *bd;
    StgPtr top;
    StgWord sizeW;

    bd = Bdescr((StgPtr)block);
    top = bd->start + BLOCK_SIZE_W * bd->blocks;

    // We consider a block full if we could not fit
    // an entire closure with 7 payload items
    // (this leaves a slop of 64 bytes at most, but
    // it avoids leaving a block almost empty to fit
    // a large byte array, while at the same time
    // it avoids trying to allocate a large closure
    // in a chain of almost empty blocks)
    sizeW = sizeofW(StgHeader) + 7;
    return (bd->free + sizeW > top);
}

static rtsBool
allocate_loop (Capability       *cap,
               StgCompactNFData *str,
               StgWord           sizeW,
               StgPtr           *at)
{
    StgCompactNFDataBlock *block;
    StgWord next_size;

    // try the nursery first
 retry:
    if (str->nursery != NULL) {
        if (allocate_in_compact(str->nursery, sizeW, at))
            return rtsTrue;

        if (block_is_full (str->nursery)) {
            str->nursery = str->nursery->next;
            goto retry;
        }

        // try subsequent blocks
        block = str->nursery->next;
        while (block != NULL) {
            if (allocate_in_compact(block, sizeW, at))
                return rtsTrue;

            block = block->next;
        }
    }

    next_size = stg_max(str->autoBlockW * sizeof(StgWord),
                    BLOCK_ROUND_UP(sizeW * sizeof(StgWord)));
    if (next_size >= BLOCKS_PER_MBLOCK * BLOCK_SIZE)
        next_size = BLOCKS_PER_MBLOCK * BLOCK_SIZE;
    if (next_size < sizeW * sizeof(StgWord) + sizeof(StgCompactNFDataBlock))
        return rtsFalse;

    block = compactAppendBlock(cap, str, next_size);
    ASSERT (str->nursery != NULL);
    return allocate_in_compact(block, sizeW, at);
}

static void
copy_tag (Capability        *cap,
          StgCompactNFData  *str,
          HashTable         *hash,
          StgClosure       **p,
          StgClosure        *from,
          StgWord            tag)
{
    StgPtr to;
    StgWord sizeW;

    sizeW = closure_sizeW(from);

    if (!allocate_loop(cap, str, sizeW, &to)) {
        barf("Failed to copy object in compact, object too large\n");
        return;
    }

    // unroll memcpy for small sizes because we can
    // benefit of known alignment
    // (32 extracted from my magic hat)
    if (sizeW < 32)
        unroll_memcpy(to, (StgPtr)from, sizeW);
    else
        memcpy(to, from, sizeW * sizeof(StgWord));

    if (hash != NULL)
        insertHashTable(hash, (StgWord)from, to);

    *p = TAG_CLOSURE(tag, (StgClosure*)to);
}

STATIC_INLINE rtsBool
object_in_compact (StgCompactNFData *str, StgClosure *p)
{
    bdescr *bd;

    if (!HEAP_ALLOCED(p))
        return rtsFalse;

    bd = Bdescr((P_)p);
    return (bd->flags & BF_COMPACT) != 0 &&
        objectGetCompact(p) == str;
}

static void
simple_evacuate (Capability        *cap,
                 StgCompactNFData  *str,
                 HashTable         *hash,
                 StgClosure       **p)
{
    StgWord tag;
    StgClosure *from;
    void *already;

    from = *p;
    tag = GET_CLOSURE_TAG(from);
    from = UNTAG_CLOSURE(from);

    // If the object referenced is already in this compact
    // (for example by reappending an object that was obtained
    // by compactGetRoot) then do nothing
    if (object_in_compact(str, from))
        return;

    switch (get_itbl(from)->type) {
    case BLACKHOLE:
        // If tag == 0, the indirectee is the TSO that claimed the tag
        //
        // Not useful and not NFData
        from = ((StgInd*)from)->indirectee;
        if (GET_CLOSURE_TAG(from) == 0) {
            debugBelch("Claimed but not updated BLACKHOLE in Compact,"
                       " not normal form");
            return;
        }

        *p = from;
        return simple_evacuate(cap, str, hash, p);

    case IND:
    case IND_STATIC:
        // follow chains of indirections, don't evacuate them
        from = ((StgInd*)from)->indirectee;
        *p = from;
        // Evac.c uses a goto, but let's rely on a smart compiler
        // and get readable code instead
        return simple_evacuate(cap, str, hash, p);

    default:
        // This object was evacuated already, return the existing
        // pointer
        if (hash != NULL &&
            (already = lookupHashTable (hash, (StgWord)from))) {
            *p = TAG_CLOSURE(tag, (StgClosure*)already);
            return;
        }

        copy_tag(cap, str, hash, p, from, tag);
    }
}

static void
simple_scavenge_mut_arr_ptrs (Capability       *cap,
                              StgCompactNFData *str,
                              HashTable        *hash,
                              StgMutArrPtrs    *a)
{
    StgPtr p, q;

    p = (StgPtr)&a->payload[0];
    q = (StgPtr)&a->payload[a->ptrs];
    for (; p < q; p++) {
        simple_evacuate(cap, str, hash, (StgClosure**)p);
    }
}

static void
simple_scavenge_block (Capability            *cap,
                       StgCompactNFData      *str,
                       StgCompactNFDataBlock *block,
                       HashTable             *hash,
                       StgPtr                 p)
{
    const StgInfoTable *info;
    bdescr *bd = Bdescr((P_)block);

    while (p < bd->free) {
        ASSERT (LOOKS_LIKE_CLOSURE_PTR(p));
        info = get_itbl((StgClosure*)p);

        switch (info->type) {
        case CONSTR_1_0:
            simple_evacuate(cap, str, hash, &((StgClosure*)p)->payload[0]);
        case CONSTR_0_1:
            p += sizeofW(StgClosure) + 1;
            break;

        case CONSTR_2_0:
            simple_evacuate(cap, str, hash, &((StgClosure*)p)->payload[1]);
        case CONSTR_1_1:
            simple_evacuate(cap, str, hash, &((StgClosure*)p)->payload[0]);
        case CONSTR_0_2:
            p += sizeofW(StgClosure) + 2;
            break;

        case CONSTR:
        case PRIM:
        case CONSTR_NOCAF_STATIC:
        case CONSTR_STATIC:
        {
            StgPtr end;

            end = (P_)((StgClosure *)p)->payload + info->layout.payload.ptrs;
            for (p = (P_)((StgClosure *)p)->payload; p < end; p++) {
                simple_evacuate(cap, str, hash, (StgClosure **)p);
            }
            p += info->layout.payload.nptrs;
            break;
        }

        case ARR_WORDS:
            p += arr_words_sizeW((StgArrBytes*)p);
            break;

        case MUT_ARR_PTRS_FROZEN:
        case MUT_ARR_PTRS_FROZEN0:
            simple_scavenge_mut_arr_ptrs(cap, str, hash, (StgMutArrPtrs*)p);
            p += mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
            break;

        case SMALL_MUT_ARR_PTRS_FROZEN:
        case SMALL_MUT_ARR_PTRS_FROZEN0:
        {
            uint32_t i;
            StgSmallMutArrPtrs *arr = (StgSmallMutArrPtrs*)p;

            for (i = 0; i < arr->ptrs; i++)
                simple_evacuate(cap, str, hash, &arr->payload[i]);

            p += sizeofW(StgSmallMutArrPtrs) + arr->ptrs;
            break;
        }

        case IND:
        case BLACKHOLE:
        case IND_STATIC:
            // They get shortcircuited by simple_evaluate()
            barf("IND/BLACKHOLE in Compact");
            break;

        default:
            barf("Invalid non-NFData closure in Compact\n");
        }
    }
}

static void
scavenge_loop (Capability            *cap,
               StgCompactNFData      *str,
               StgCompactNFDataBlock *first_block,
               HashTable             *hash,
               StgPtr                 p)
{
    // Scavenge the first block
    simple_scavenge_block(cap, str, first_block, hash, p);

    // Note: simple_scavenge_block can change str->last, which
    // changes this check, in addition to iterating through
    while (first_block != str->last) {
        // we can't allocate in blocks that were already scavenged
        // so push the nursery forward
        if (str->nursery == first_block)
            str->nursery = str->nursery->next;

        first_block = first_block->next;
        simple_scavenge_block(cap, str, first_block, hash,
                              (P_)first_block + sizeofW(StgCompactNFDataBlock));
    }
}

#ifdef DEBUG
static rtsBool
objectIsWHNFData (StgClosure *what)
{
    switch (get_itbl(what)->type) {
    case CONSTR:
    case CONSTR_1_0:
    case CONSTR_0_1:
    case CONSTR_2_0:
    case CONSTR_1_1:
    case CONSTR_0_2:
    case CONSTR_STATIC:
    case CONSTR_NOCAF_STATIC:
    case ARR_WORDS:
    case MUT_ARR_PTRS_FROZEN:
    case MUT_ARR_PTRS_FROZEN0:
    case SMALL_MUT_ARR_PTRS_FROZEN:
    case SMALL_MUT_ARR_PTRS_FROZEN0:
        return rtsTrue;

    case IND:
    case BLACKHOLE:
        return objectIsWHNFData(UNTAG_CLOSURE(((StgInd*)what)->indirectee));

    default:
        return rtsFalse;
    }
}

static rtsBool
verify_mut_arr_ptrs (StgCompactNFData *str,
                     StgMutArrPtrs    *a)
{
    StgPtr p, q;

    p = (StgPtr)&a->payload[0];
    q = (StgPtr)&a->payload[a->ptrs];
    for (; p < q; p++) {
        if (!object_in_compact(str, UNTAG_CLOSURE(*(StgClosure**)p)))
            return rtsFalse;
    }

    return rtsTrue;
}

static rtsBool
verify_consistency_block (StgCompactNFData *str, StgCompactNFDataBlock *block)
{
    bdescr *bd;
    StgPtr p;
    const StgInfoTable *info;
    StgClosure *q;

    p = (P_)firstBlockGetCompact(block);
    bd = Bdescr((P_)block);
    while (p < bd->free) {
        q = (StgClosure*)p;

        if (!LOOKS_LIKE_CLOSURE_PTR(q))
            return rtsFalse;

        info = get_itbl(q);
        switch (info->type) {
        case CONSTR_1_0:
            if (!object_in_compact(str, UNTAG_CLOSURE(q->payload[0])))
                return rtsFalse;
        case CONSTR_0_1:
            p += sizeofW(StgClosure) + 1;
            break;

        case CONSTR_2_0:
            if (!object_in_compact(str, UNTAG_CLOSURE(q->payload[1])))
                return rtsFalse;
        case CONSTR_1_1:
            if (!object_in_compact(str, UNTAG_CLOSURE(q->payload[0])))
                return rtsFalse;
        case CONSTR_0_2:
            p += sizeofW(StgClosure) + 2;
            break;

        case CONSTR:
        case PRIM:
        case CONSTR_STATIC:
        case CONSTR_NOCAF_STATIC:
        {
            uint32_t i;

            for (i = 0; i < info->layout.payload.ptrs; i++)
                if (!object_in_compact(str, UNTAG_CLOSURE(q->payload[i])))
                    return rtsFalse;

            p += sizeofW(StgClosure) + info->layout.payload.ptrs +
                info->layout.payload.nptrs;
            break;
        }

        case ARR_WORDS:
            p += arr_words_sizeW((StgArrBytes*)p);
            break;

        case MUT_ARR_PTRS_FROZEN:
        case MUT_ARR_PTRS_FROZEN0:
            if (!verify_mut_arr_ptrs(str, (StgMutArrPtrs*)p))
                return rtsFalse;
            p += mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
            break;

        case SMALL_MUT_ARR_PTRS_FROZEN:
        case SMALL_MUT_ARR_PTRS_FROZEN0:
        {
            uint32_t i;
            StgSmallMutArrPtrs *arr = (StgSmallMutArrPtrs*)p;

            for (i = 0; i < arr->ptrs; i++)
                if (!object_in_compact(str, UNTAG_CLOSURE(arr->payload[i])))
                    return rtsFalse;

            p += sizeofW(StgSmallMutArrPtrs) + arr->ptrs;
            break;
        }

        case COMPACT_NFDATA:
            p += sizeofW(StgCompactNFData);
            break;

        default:
            return rtsFalse;
        }
    }

    return rtsTrue;
}

static rtsBool
verify_consistency_loop (StgCompactNFData *str)
{
    StgCompactNFDataBlock *block;

    block = compactGetFirstBlock(str);
    do {
        if (!verify_consistency_block(str, block))
            return rtsFalse;
        block = block->next;
    } while (block && block->owner);

    return rtsTrue;
}
#endif


StgPtr
compactAppend (Capability       *cap,
               StgCompactNFData *str,
               StgClosure       *what,
               StgWord           share)
{
    StgClosure *root;
    StgClosure *tagged_root;
    HashTable *hash;
    StgCompactNFDataBlock *evaced_block;

    ASSERT(objectIsWHNFData(UNTAG_CLOSURE(what)));

    tagged_root = what;
    simple_evacuate(cap, str, NULL, &tagged_root);

    root = UNTAG_CLOSURE(tagged_root);
    evaced_block = objectGetCompactBlock(root);

    if (share) {
        hash = allocHashTable ();
        insertHashTable(hash, (StgWord)UNTAG_CLOSURE(what), root);
    } else
        hash = NULL;

    scavenge_loop(cap, str, evaced_block, hash, (P_)root);

    if (share)
        freeHashTable(hash, NULL);

    ASSERT(verify_consistency_loop(str));

    return (StgPtr)tagged_root;
}

StgWord
compactContains (StgCompactNFData *str, StgPtr what)
{
    bdescr *bd;

    // This check is the reason why this needs to be
    // implemented in C instead of (possibly faster) Cmm
    if (!HEAP_ALLOCED (what))
        return 0;

    // Note that we don't care about tags, they are eaten
    // away by the Bdescr operation anyway
    bd = Bdescr((P_)what);
    return (bd->flags & BF_COMPACT) != 0 &&
        (str == NULL || objectGetCompact((StgClosure*)what) == str);
}

StgCompactNFDataBlock *
compactAllocateBlock(Capability            *cap,
                     StgWord                size,
                     StgCompactNFDataBlock *previous)
{
    StgWord aligned_size;
    StgCompactNFDataBlock *block;
    bdescr *bd;

    aligned_size = BLOCK_ROUND_UP(size);

    // We do not link the new object into the generation ever
    // - we cannot let the GC know about this object until we're done
    // importing it and we have fixed up all info tables and stuff
    //
    // but we do update n_compact_blocks, otherwise memInventory()
    // in Sanity will think we have a memory leak, because it compares
    // the blocks he knows about with the blocks obtained by the
    // block allocator
    // (if by chance a memory leak does happen due to a bug somewhere
    // else, memInventory will also report that all compact blocks
    // associated with this compact are leaked - but they are not really,
    // we have a pointer to them and we're not losing track of it, it's
    // just we can't use the GC until we're done with the import)
    //
    // (That btw means that the high level import code must be careful
    // not to lose the pointer, so don't use the primops directly
    // unless you know what you're doing!)

    // Other trickery: we pass NULL as first, which means our blocks
    // are always in generation 0
    // This is correct because the GC has never seen the blocks so
    // it had no chance of promoting them

    block = compactAllocateBlockInternal(cap, aligned_size, NULL,
                                         previous != NULL ? ALLOCATE_IMPORT_APPEND : ALLOCATE_IMPORT_NEW);
    if (previous != NULL)
        previous->next = block;

    bd = Bdescr((P_)block);
    bd->free = (P_)((W_)bd->start + size);

    return block;
}

STATIC_INLINE rtsBool
any_needs_fixup(StgCompactNFDataBlock *block)
{
    // ->next pointers are always valid, even if some blocks were
    // not allocated where we want them, because compactAllocateAt()
    // will take care to adjust them

    do {
        if (block->self != block)
            return rtsTrue;
        block = block->next;
    } while (block && block->owner);

    return rtsFalse;
}

#ifdef DEBUG
static void
spew_failing_pointer(StgWord *fixup_table, uint32_t count, StgWord address)
{
    uint32_t i;
    StgWord key, value;
    StgCompactNFDataBlock *block;
    bdescr *bd;
    StgWord size;

    debugBelch("Failed to adjust 0x%" FMT_HexWord ". Block dump follows...\n",
               address);

    for (i  = 0; i < count; i++) {
        key = fixup_table [2 * i];
        value = fixup_table [2 * i + 1];

        block = (StgCompactNFDataBlock*)value;
        bd = Bdescr((P_)block);
        size = (W_)bd->free - (W_)bd->start;

        debugBelch("%" FMT_Word32 ": was 0x%" FMT_HexWord "-0x%" FMT_HexWord
                   ", now 0x%" FMT_HexWord "-0x%" FMT_HexWord "\n", i, key,
                   key+size, value, value+size);
    }
}
#endif

STATIC_INLINE StgCompactNFDataBlock *
find_pointer(StgWord *fixup_table, uint32_t count, StgClosure *q)
{
    StgWord address = (W_)q;
    uint32_t a, b, c;
    StgWord key, value;
    bdescr *bd;

    a = 0;
    b = count;
    while (a < b-1) {
        c = (a+b)/2;

        key = fixup_table[c * 2];
        value = fixup_table[c * 2 + 1];

        if (key > address)
            b = c;
        else
            a = c;
    }

    // three cases here: 0, 1 or 2 blocks to check
    for ( ; a < b; a++) {
        key = fixup_table[a * 2];
        value = fixup_table[a * 2 + 1];

        if (key > address)
            goto fail;

        bd = Bdescr((P_)value);

        if (key + bd->blocks * BLOCK_SIZE <= address)
            goto fail;

        return (StgCompactNFDataBlock*)value;
    }

 fail:
    // We should never get here

#ifdef DEBUG
    spew_failing_pointer(fixup_table, count, address);
#endif
    return NULL;
}

static rtsBool
fixup_one_pointer(StgWord *fixup_table, uint32_t count, StgClosure **p)
{
    StgWord tag;
    StgClosure *q;
    StgCompactNFDataBlock *block;

    q = *p;
    tag = GET_CLOSURE_TAG(q);
    q = UNTAG_CLOSURE(q);

    block = find_pointer(fixup_table, count, q);
    if (block == NULL)
        return rtsFalse;
    if (block == block->self)
        return rtsTrue;

    q = (StgClosure*)((W_)q - (W_)block->self + (W_)block);
    *p = TAG_CLOSURE(tag, q);

    return rtsTrue;
}

static rtsBool
fixup_mut_arr_ptrs (StgWord          *fixup_table,
                    uint32_t               count,
                    StgMutArrPtrs    *a)
{
    StgPtr p, q;

    p = (StgPtr)&a->payload[0];
    q = (StgPtr)&a->payload[a->ptrs];
    for (; p < q; p++) {
        if (!fixup_one_pointer(fixup_table, count, (StgClosure**)p))
            return rtsFalse;
    }

    return rtsTrue;
}

static rtsBool
fixup_block(StgCompactNFDataBlock *block, StgWord *fixup_table, uint32_t count)
{
    const StgInfoTable *info;
    bdescr *bd;
    StgPtr p;

    bd = Bdescr((P_)block);
    p = bd->start + sizeofW(StgCompactNFDataBlock);
    while (p < bd->free) {
        ASSERT (LOOKS_LIKE_CLOSURE_PTR(p));
        info = get_itbl((StgClosure*)p);

        switch (info->type) {
        case CONSTR_1_0:
            if (!fixup_one_pointer(fixup_table, count,
                                   &((StgClosure*)p)->payload[0]))
                return rtsFalse;
        case CONSTR_0_1:
            p += sizeofW(StgClosure) + 1;
            break;

        case CONSTR_2_0:
            if (!fixup_one_pointer(fixup_table, count,
                                   &((StgClosure*)p)->payload[1]))
                return rtsFalse;
        case CONSTR_1_1:
            if (!fixup_one_pointer(fixup_table, count,
                                   &((StgClosure*)p)->payload[0]))
                return rtsFalse;
        case CONSTR_0_2:
            p += sizeofW(StgClosure) + 2;
            break;

        case CONSTR:
        case PRIM:
        case CONSTR_STATIC:
        case CONSTR_NOCAF_STATIC:
        {
            StgPtr end;

            end = (P_)((StgClosure *)p)->payload + info->layout.payload.ptrs;
            for (p = (P_)((StgClosure *)p)->payload; p < end; p++) {
                if (!fixup_one_pointer(fixup_table, count, (StgClosure **)p))
                    return rtsFalse;
            }
            p += info->layout.payload.nptrs;
            break;
        }

        case ARR_WORDS:
            p += arr_words_sizeW((StgArrBytes*)p);
            break;

        case MUT_ARR_PTRS_FROZEN:
        case MUT_ARR_PTRS_FROZEN0:
            fixup_mut_arr_ptrs(fixup_table, count, (StgMutArrPtrs*)p);
            p += mut_arr_ptrs_sizeW((StgMutArrPtrs*)p);
            break;

        case SMALL_MUT_ARR_PTRS_FROZEN:
        case SMALL_MUT_ARR_PTRS_FROZEN0:
        {
            uint32_t i;
            StgSmallMutArrPtrs *arr = (StgSmallMutArrPtrs*)p;

            for (i = 0; i < arr->ptrs; i++) {
                if (!fixup_one_pointer(fixup_table, count,
                                       &arr->payload[i]))
                    return rtsFalse;
            }

            p += sizeofW(StgSmallMutArrPtrs) + arr->ptrs;
            break;
        }

        case COMPACT_NFDATA:
            if (p == (bd->start + sizeofW(StgCompactNFDataBlock))) {
                // Ignore the COMPACT_NFDATA header
                // (it will be fixed up later)
                p += sizeofW(StgCompactNFData);
                break;
            }

            // fall through

        default:
            debugBelch("Invalid non-NFData closure (type %d) in Compact\n",
                       info->type);
            return rtsFalse;
        }
    }

    return rtsTrue;
}

static int
cmp_fixup_table_item (const void *e1, const void *e2)
{
    const StgWord *w1 = e1;
    const StgWord *w2 = e2;

    return *w1 - *w2;
}

static StgWord *
build_fixup_table (StgCompactNFDataBlock *block, uint32_t *pcount)
{
    uint32_t count;
    StgCompactNFDataBlock *tmp;
    StgWord *table;

    count = 0;
    tmp = block;
    do {
        count++;
        tmp = tmp->next;
    } while(tmp && tmp->owner);

    table = stgMallocBytes(sizeof(StgWord) * 2 * count, "build_fixup_table");

    count = 0;
    do {
        table[count * 2] = (W_)block->self;
        table[count * 2 + 1] = (W_)block;
        count++;
        block = block->next;
    } while(block && block->owner);

    qsort(table, count, sizeof(StgWord) * 2, cmp_fixup_table_item);

    *pcount = count;
    return table;
}

static rtsBool
fixup_loop(StgCompactNFDataBlock *block, StgClosure **proot)
{
    StgWord *table;
    rtsBool ok;
    uint32_t count;

    table = build_fixup_table (block, &count);

    do {
        if (!fixup_block(block, table, count)) {
            ok = rtsFalse;
            goto out;
        }

        block = block->next;
    } while(block && block->owner);

    ok = fixup_one_pointer(table, count, proot);

 out:
    stgFree(table);
    return ok;
}

static void
fixup_early(StgCompactNFData *str, StgCompactNFDataBlock *block)
{
    StgCompactNFDataBlock *last;

    do {
        last = block;
        block = block->next;
    } while(block);

    str->last = last;
}

static void
fixup_late(StgCompactNFData *str, StgCompactNFDataBlock *block)
{
    StgCompactNFDataBlock *nursery;
    bdescr *bd;
    StgWord totalW;
    StgWord totalDataW;

    nursery = block;
    totalW = 0;
    totalDataW = 0;
    do {
        block->self = block;

        bd = Bdescr((P_)block);
        totalW += bd->blocks * BLOCK_SIZE_W;

        if (block->owner != NULL) {
            if (bd->free != bd->start)
                nursery = block;
            block->owner = str;
            totalDataW += bd->blocks * BLOCK_SIZE_W;
        }

        block = block->next;
    } while(block);

    str->nursery = nursery;
    str->totalW = totalW;
    str->totalDataW = totalDataW;
}

static StgClosure *
maybe_fixup_internal_pointers (StgCompactNFDataBlock *block,
                               StgClosure            *root)
{
    rtsBool ok;
    StgClosure **proot;

    // Check for fast path
    if (!any_needs_fixup(block))
        return root;

    debugBelch("Compact imported at the wrong address, will fix up"
               " internal pointers\n");

    // I am PROOT!
    proot = &root;

    ok = fixup_loop(block, proot);
    if (!ok)
        *proot = NULL;

    return *proot;
}

StgPtr
compactFixupPointers(StgCompactNFData *str,
                     StgClosure       *root)
{
    StgCompactNFDataBlock *block;
    bdescr *bd;
    StgWord total_blocks;

    block = compactGetFirstBlock(str);

    fixup_early(str, block);

    root = maybe_fixup_internal_pointers(block, root);

    // Do the late fixup even if we did not fixup all
    // internal pointers, we need that for GC and Sanity
    fixup_late(str, block);

    // Now we're ready to let the GC, Sanity, the profiler
    // etc. know about this object
    bd = Bdescr((P_)block);

    total_blocks = str->totalW / BLOCK_SIZE_W;

    ACQUIRE_SM_LOCK;
    ASSERT (bd->gen == g0);
    ASSERT (g0->n_compact_blocks_in_import >= total_blocks);
    g0->n_compact_blocks_in_import -= total_blocks;
    g0->n_compact_blocks += total_blocks;
    dbl_link_remove(bd, &g0->compact_blocks_in_import);
    dbl_link_onto(bd, &g0->compact_objects);
    RELEASE_SM_LOCK;

#if DEBUG
    if (root)
        verify_consistency_loop(str);
#endif

    return (StgPtr)root;
}
