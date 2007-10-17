/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2007
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
int mblock_map_count = 0;
MBlockMap **mblock_maps = NULL;

static MBlockMap *
findMBlockMap(void *p)
{
    int i;
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
