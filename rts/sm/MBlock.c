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
#include "BlockAlloc.h"
#include "Trace.h"
#include "OSMem.h"

#include <string.h>

W_ peak_mblocks_allocated = 0;
W_ mblocks_allocated = 0;
W_ mpc_misses = 0;

/* -----------------------------------------------------------------------------
   The MBlock Map: provides our implementation of HEAP_ALLOCED()
   -------------------------------------------------------------------------- */

#if SIZEOF_VOID_P == 4
StgWord8 mblock_map[MBLOCK_MAP_SIZE]; // initially all zeros

static void
setHeapAlloced(void *p, StgWord8 i)
{
    mblock_map[MBLOCK_MAP_ENTRY(p)] = i;
}

#elif SIZEOF_VOID_P == 8

MBlockMap **mblock_maps = NULL;

nat mblock_map_count = 0;

MbcCacheLine mblock_cache[MBC_ENTRIES];

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

StgBool HEAP_ALLOCED_miss(StgWord mblock, void *p)
{
    MBlockMap *map;
    MBlockMapLine value;
    nat entry_no;
    
    entry_no = mblock & (MBC_ENTRIES-1);

    map = findMBlockMap(p);
    if (map)
    {
        mpc_misses++;
        value = map->lines[MBLOCK_MAP_LINE(p)];
        mblock_cache[entry_no] = (mblock<<1) | value;
        return value;
    }
    else
    {
        mblock_cache[entry_no] = (mblock<<1);
        return 0;
    }
}

static void
setHeapAlloced(void *p, StgWord8 i)
{
    MBlockMap *map = findMBlockMap(p);
    if(map == NULL)
    {
    	mblock_map_count++;
    	mblock_maps = stgReallocBytes(mblock_maps,
                                      sizeof(MBlockMap*) * mblock_map_count,
                                      "markHeapAlloced(1)");
	map = mblock_maps[mblock_map_count-1] = 
            stgMallocBytes(sizeof(MBlockMap),"markHeapAlloced(2)");
        memset(map,0,sizeof(MBlockMap));
	map->addrHigh32 = (StgWord32) (((StgWord)p) >> 32);
    }

    map->lines[MBLOCK_MAP_LINE(p)] = i;

    {
        StgWord mblock;
        nat entry_no;
        
        mblock   = (StgWord)p >> MBLOCK_SHIFT;
        entry_no = mblock & (MBC_ENTRIES-1);
        mblock_cache[entry_no] = (mblock << 1) + i;
    }
}
#endif

static void
markHeapAlloced(void *p)
{
    setHeapAlloced(p, 1);
}

static void
markHeapUnalloced(void *p)
{
    setHeapAlloced(p, 0);
}

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

void * getNextMBlock(void *p)
{
    MBlockMap *map;
    nat off, j;
    nat line_no;
    MBlockMapLine line;

    for (j = 0; j < mblock_map_count; j++)  {
        map = mblock_maps[j];
        if (map->addrHigh32 == (StgWord)p >> 32) break;
    }
    if (j == mblock_map_count) return NULL;

    for (; j < mblock_map_count; j++) {
        map = mblock_maps[j];
        if (map->addrHigh32 == (StgWord)p >> 32) {
            line_no = MBLOCK_MAP_LINE(p);
            off  = (((StgWord)p >> MBLOCK_SHIFT) & (MBC_LINE_SIZE-1)) + 1;
            // + 1 because we want the *next* mblock
        } else {
            line_no = 0; off = 0;
        }
        for (; line_no < MBLOCK_MAP_ENTRIES; line_no++) {
            line = map->lines[line_no];
            for (; off < MBC_LINE_SIZE; off++) {
                if (line & (1<<off)) {
                    return (void*)(((StgWord)map->addrHigh32 << 32) + 
                                   line_no * MBC_LINE_SIZE * MBLOCK_SIZE +
                                   off * MBLOCK_SIZE);
                }
            }
            off = 0;
        }
    }
    return NULL;
}

void * getFirstMBlock(void)
{
    MBlockMap *map = mblock_maps[0];
    nat line_no, off;
    MbcCacheLine line;

    for (line_no = 0; line_no < MBLOCK_MAP_ENTRIES; line_no++) {
        line = map->lines[line_no];
        if (line) {
            for (off = 0; off < MBC_LINE_SIZE; off++) {
                if (line & (1<<off)) {
                    return (void*)(((StgWord)map->addrHigh32 << 32) + 
                                   line_no * MBC_LINE_SIZE * MBLOCK_SIZE +
                                   off * MBLOCK_SIZE);
                }
            }
        }
    }
    return NULL;
}

#endif // SIZEOF_VOID_P

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
        markHeapAlloced( (StgWord8*)ret + i * MBLOCK_SIZE );
    }
    
    mblocks_allocated += n;
    peak_mblocks_allocated = stg_max(peak_mblocks_allocated, mblocks_allocated);

    return ret;
}

void
freeMBlocks(void *addr, nat n)
{
    nat i;

    debugTrace(DEBUG_gc, "freeing %d megablock(s) at %p",n,addr);

    mblocks_allocated -= n;

    for (i = 0; i < n; i++) {
        markHeapUnalloced( (StgWord8*)addr + i * MBLOCK_SIZE );
    }

    osFreeMBlocks(addr, n);
}

void
freeAllMBlocks(void)
{
    debugTrace(DEBUG_gc, "freeing all megablocks");

    osFreeAllMBlocks();

#if SIZEOF_VOID_P == 8
    nat n;
    for (n = 0; n < mblock_map_count; n++) {
        stgFree(mblock_maps[n]);
    }
    stgFree(mblock_maps);
#endif
}

void
initMBlocks(void)
{
    osMemInit();
#if SIZEOF_VOID_P == 8
    memset(mblock_cache,0xff,sizeof(mblock_cache));
#endif
}
