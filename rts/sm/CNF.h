/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2014
 *
 * GC support for immutable non-GCed structures
 *
 * Documentation on the architecture of the Garbage Collector can be
 * found in the online commentary:
 *
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/rts/storage/gc
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

StgCompactNFData *compactNew   (Capability      *cap,
                                StgWord          size);
void              compactResize(Capability       *cap,
                                StgCompactNFData *str,
                                StgWord           new_size);
void              compactFree  (StgCompactNFData *str);
void              compactMarkKnown(StgCompactNFData *str);
StgWord           compactContains(StgCompactNFData *str,
                                  StgPtr            what);
StgWord           countCompactBlocks(bdescr *outer);

#if defined(DEBUG)
StgWord           countAllocdCompactBlocks(bdescr *outer);
#endif

StgCompactNFDataBlock *compactAllocateBlock(Capability            *cap,
                                            StgWord                size,
                                            StgCompactNFDataBlock *previous);
StgPtr                 compactFixupPointers(StgCompactNFData      *str,
                                            StgClosure            *root);

// Go from an arbitrary pointer into any block of a compact chain, to the
// StgCompactNFDataBlock at the beginning of the block.
INLINE_HEADER StgCompactNFDataBlock *objectGetCompactBlock (StgClosure *closure);
INLINE_HEADER StgCompactNFDataBlock *objectGetCompactBlock (StgClosure *closure)
{
    bdescr *object_block, *head_block;

    object_block = Bdescr((StgPtr)closure);

    ASSERT((object_block->flags & BF_COMPACT) != 0);

    if (object_block->blocks == 0)
        head_block = object_block->link;
    else
        head_block = object_block;

    ASSERT((head_block->flags & BF_COMPACT) != 0);

    return (StgCompactNFDataBlock*)(bdescr_start(head_block));
}

// Go from an arbitrary pointer into any block of a compact chain, to the
// StgCompactNFData for the whole compact chain.
INLINE_HEADER StgCompactNFData *objectGetCompact (StgClosure *closure);
INLINE_HEADER StgCompactNFData *objectGetCompact (StgClosure *closure)
{
    StgCompactNFDataBlock *block = objectGetCompactBlock (closure);
    return block->owner;
}

extern void *allocateForCompact (Capability *cap,
                                 StgCompactNFData *str,
                                 StgWord sizeW);

extern void insertCompactHash (Capability *cap,
                               StgCompactNFData *str,
                               StgClosure *p, StgClosure *to);

extern void verifyCompact (StgCompactNFData *str);

#include "EndPrivate.h"
