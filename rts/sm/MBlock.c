/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2008
 *
 * MegaBlock Allocator Interface.  This file contains all the dirty
 * architecture-dependent hackery required to get a chunk of aligned
 * memory from the operating system.
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"

#include "Rts.h"
#include "RtsUtils.h"
#include "MBlock.h"
#include "BlockAlloc.h"
#include "Trace.h"
#include "OSMem.h"

lnat mblocks_allocated = 0;

void
initMBlocks(void)
{
    osMemInit();
}

/* -----------------------------------------------------------------------------
   The MBlock Map: provides our implementation of HEAP_ALLOCED()
   -------------------------------------------------------------------------- */

#if SIZEOF_VOID_P == 4
StgWord8 mblock_map[MBLOCK_MAP_SIZE]; // initially all zeros
#elif SIZEOF_VOID_P == 8
static MBlockMap dummy_mblock_map;
MBlockMap *mblock_cache = &dummy_mblock_map;
nat mblock_map_count = 0;
MBlockMap **mblock_maps = NULL;

static MBlockMap *
findMBlockMap(void *p)
{
    nat i;
    StgWord32 hi = (StgWord32) (((StgWord)p) >> 32);
    for( i = 0; i < mblock_map_count; i++ )
    {
        if(mblock_maps[i]->addrHigh32 == hi)
        {
	    return mblock_maps[i];
	}
    }
    return NULL;
}

StgBool
slowIsHeapAlloced(void *p)
{
    MBlockMap *map = findMBlockMap(p);
    if(map)
    {
    	mblock_cache = map;
	return map->mblocks[MBLOCK_MAP_ENTRY(p)];
    }
    else
    	return 0;
}
#endif

static void
markHeapAlloced(void *p)
{
#if SIZEOF_VOID_P == 4
    mblock_map[MBLOCK_MAP_ENTRY(p)] = 1;
#elif SIZEOF_VOID_P == 8
    MBlockMap *map = findMBlockMap(p);
    if(map == NULL)
    {
    	mblock_map_count++;
    	mblock_maps = realloc(mblock_maps,
			      sizeof(MBlockMap*) * mblock_map_count);
	map = mblock_maps[mblock_map_count-1] = calloc(1,sizeof(MBlockMap));
	map->addrHigh32 = (StgWord32) (((StgWord)p) >> 32);
    }
    map->mblocks[MBLOCK_MAP_ENTRY(p)] = 1;
    mblock_cache = map;
#endif
}

/* ----------------------------------------------------------------------------
   Debugging code for traversing the allocated MBlocks
   
   This is used for searching for lost blocks when a memory leak is
   detected; see Blocks.c:findUnmarkedBlock().
   ------------------------------------------------------------------------ */

#ifdef DEBUG

#if SIZEOF_VOID_P == 4

STATIC_INLINE
void * mapEntryToMBlock(nat i)
{
    return (void *)((StgWord)i << MBLOCK_SHIFT);
}

void * getFirstMBlock(void)
{
    nat i;

    for (i = 0; i < MBLOCK_MAP_SIZE; i++) {
        if (mblock_map[i]) return mapEntryToMBlock(i);
    }
    return NULL;
}

void * getNextMBlock(void *mblock)
{
    nat i;

    for (i = MBLOCK_MAP_ENTRY(mblock) + 1; i < MBLOCK_MAP_SIZE; i++) {
        if (mblock_map[i]) return mapEntryToMBlock(i);
    }
    return NULL;
}

#elif SIZEOF_VOID_P == 8

STATIC_INLINE
void * mapEntryToMBlock(MBlockMap *map, nat i)
{
    return (void *)(((StgWord)map->addrHigh32) << 32) + 
        ((StgWord)i << MBLOCK_SHIFT);
}

void * getFirstMBlock(void)
{
    MBlockMap *map;
    nat i, j;

    for (j = 0; j < mblock_map_count; j++)  {
        map = mblock_maps[j];
        for (i = 0; i < MBLOCK_MAP_SIZE; i++) {
            if (map->mblocks[i]) return mapEntryToMBlock(map,i);
        }
    }
    return NULL;
}

void * getNextMBlock(void *mblock)
{
    MBlockMap *map;
    nat i, j;

    for (j = 0; j < mblock_map_count; j++)  {
        map = mblock_maps[j];
        if (map->addrHigh32 == (StgWord)mblock >> 32) break;
    }
    if (j == mblock_map_count) return NULL;

    for (; j < mblock_map_count; j++) {
        map = mblock_maps[j];
        if (map->addrHigh32 == (StgWord)mblock >> 32) {
            i = MBLOCK_MAP_ENTRY(mblock) + 1;
        } else {
            i = 0;
        }
        for (; i < MBLOCK_MAP_SIZE; i++) {
            if (map->mblocks[i]) return mapEntryToMBlock(map,i);
        }
    }
    return NULL;
}

#endif // SIZEOF_VOID_P

#endif // DEBUG

/* -----------------------------------------------------------------------------
   Allocate new mblock(s)
   -------------------------------------------------------------------------- */

void *
getMBlock(void)
{
  return getMBlocks(1);
}

// The external interface: allocate 'n' mblocks, and return the
// address.

void *
getMBlocks(nat n)
{
    nat i;
    void *ret;

    ret = osGetMBlocks(n);

    debugTrace(DEBUG_gc, "allocated %d megablock(s) at %p",n,ret);
    
    // fill in the table
    for (i = 0; i < n; i++) {
        markHeapAlloced( ret + i * MBLOCK_SIZE );
    }
    
    mblocks_allocated += n;

    return ret;
}

void
freeAllMBlocks(void)
{
    osFreeAllMBlocks();
}
