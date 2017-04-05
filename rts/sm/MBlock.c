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
   The MBlock Map: provides our implementation of HEAP_ALLOCED() and the
   utilities to walk the really allocated (thus accessible without risk of
   segfault) heap
   -------------------------------------------------------------------------- */

/*
  There are two different cases here: either we use "large address
  space" (which really means two-step allocation), so we have to
  manage which memory is good (= accessible without fear of segfault)
  and which is not owned by us, or we use the older method and get
  good memory straight from the system.

  Both code paths need to provide:

  void *getFirstMBlock(void ** state)
      return the first (lowest address) mblock
      that was actually committed

  void *getNextMBlock(void ** state, void * mblock)
      return the first (lowest address) mblock
      that was committed, after the given one

  For both these calls, @state is an in-out parameter that points to
  an opaque state threading the calls togheter. The calls should only
  be used in an interation fashion. Pass NULL if @state is not
  interesting,or pass a pointer to NULL if you don't have a state.

  void *getCommittedMBlocks(uint32_t n)
      return @n new mblocks, ready to be used (reserved and committed)

  void *decommitMBlocks(char *addr, uint32_t n)
      release memory for @n mblocks, starting at the given address

  void releaseFreeMemory()
      potentially release any address space that was associated
      with recently decommitted blocks
*/

#ifdef USE_LARGE_ADDRESS_SPACE

// Large address space means we use two-step allocation: reserve
// something large upfront, and then commit as needed
// (This is normally only useful on 64-bit, where we can assume
// that reserving 1TB is possible)
//
// There is no block map in this case, but there is a free list
// of blocks that were committed and decommitted at least once,
// which we use to choose which block to commit next in the already
// reserved space.
//
// We cannot let the OS choose it as we do in the
// non large address space case, because the committing wants to
// know the exact address upfront.
//
// The free list is coalesced and ordered, which means that
// allocate and free are worst-case O(n), but benchmarks have shown
// that this is not a significant problem, because large (>=2MB)
// allocations are infrequent and their time is mostly insignificant
// compared to the time to use that memory.
//
// The free list is stored in malloc()'d memory, unlike the other free
// lists in BlockAlloc.c which are stored in block descriptors,
// because we cannot touch the contents of decommitted mblocks.

typedef struct free_list {
    struct free_list *prev;
    struct free_list *next;
    W_ address;
    W_ size;
} free_list;

static free_list *free_list_head;
static W_ mblock_high_watermark;
/*
 * it is quite important that these are in the same cache line as they
 * are both needed by HEAP_ALLOCED. Moreover, we need to ensure that they
 * don't share a cache line with anything else to prevent false sharing.
 */
struct mblock_address_range mblock_address_space = { 0, 0, {} };

static void *getAllocatedMBlock(free_list **start_iter, W_ startingAt)
{
    free_list *iter;
    W_ p = startingAt;

    for (iter = *start_iter; iter != NULL; iter = iter->next)
    {
        if (p < iter->address)
            break;

        if (p == iter->address)
            p += iter->size;
    }

    *start_iter = iter;

    if (p >= mblock_high_watermark)
        return NULL;

    return (void*)p;
}

void * getFirstMBlock(void **state STG_UNUSED)
{
    free_list *fake_state;
    free_list **casted_state;

    if (state)
        casted_state = (free_list**)state;
    else
        casted_state = &fake_state;

    *casted_state = free_list_head;
    return getAllocatedMBlock(casted_state, mblock_address_space.begin);
}

void * getNextMBlock(void **state STG_UNUSED, void *mblock)
{
    free_list *fake_state = free_list_head;
    free_list **casted_state;

    if (state)
        casted_state = (free_list**)state;
    else
        casted_state = &fake_state;

    return getAllocatedMBlock(casted_state, (W_)mblock + MBLOCK_SIZE);
}

static void *getReusableMBlocks(uint32_t n)
{
    struct free_list *iter;
    W_ size = MBLOCK_SIZE * (W_)n;

    for (iter = free_list_head; iter != NULL; iter = iter->next) {
        void *addr;

        if (iter->size < size)
            continue;

        addr = (void*)iter->address;
        iter->address += size;
        iter->size -= size;
        if (iter->size == 0) {
            struct free_list *prev, *next;

            prev = iter->prev;
            next = iter->next;
            if (prev == NULL) {
                ASSERT(free_list_head == iter);
                free_list_head = next;
            } else {
                prev->next = next;
            }
            if (next != NULL) {
                next->prev = prev;
            }
            stgFree(iter);
        }

        osCommitMemory(addr, size);
        return addr;
    }

    return NULL;
}

static void *getFreshMBlocks(uint32_t n)
{
    W_ size = MBLOCK_SIZE * (W_)n;
    void *addr = (void*)mblock_high_watermark;

    if (mblock_high_watermark + size > mblock_address_space.end)
    {
        // whoa, 1 TB of heap?
        errorBelch("out of memory");
        stg_exit(EXIT_HEAPOVERFLOW);
    }

    osCommitMemory(addr, size);
    mblock_high_watermark += size;
    return addr;
}

static void *getCommittedMBlocks(uint32_t n)
{
    void *p;

    p = getReusableMBlocks(n);
    if (p == NULL) {
        p = getFreshMBlocks(n);
    }

    ASSERT(p != NULL && p != (void*)-1);
    return p;
}

static void decommitMBlocks(char *addr, uint32_t n)
{
    struct free_list *iter, *prev;
    W_ size = MBLOCK_SIZE * (W_)n;
    W_ address = (W_)addr;

    osDecommitMemory(addr, size);

    prev = NULL;
    for (iter = free_list_head; iter != NULL; iter = iter->next)
    {
        prev = iter;

        if (iter->address + iter->size < address)
            continue;

        if (iter->address + iter->size == address) {
            iter->size += size;

            if (address + size == mblock_high_watermark) {
                mblock_high_watermark -= iter->size;
                if (iter->prev) {
                    iter->prev->next = NULL;
                } else {
                    ASSERT(iter == free_list_head);
                    free_list_head = NULL;
                }
                stgFree(iter);
                return;
            }

            if (iter->next &&
                iter->next->address == iter->address + iter->size) {
                struct free_list *next;

                next = iter->next;
                iter->size += next->size;
                iter->next = next->next;

                if (iter->next) {
                    iter->next->prev = iter;

                    /* We don't need to consolidate more */
                    ASSERT(iter->next->address > iter->address + iter->size);
                }

                stgFree(next);
            }
            return;
        } else if (address + size == iter->address) {
            iter->address = address;
            iter->size += size;

            /* We don't need to consolidate backwards
               (because otherwise it would have been handled by
               the previous iteration) */
            if (iter->prev) {
                ASSERT(iter->prev->address + iter->prev->size < iter->address);
            }
            return;
        } else {
            struct free_list *new_iter;

            /* All other cases have been handled */
            ASSERT(iter->address > address + size);

            new_iter = stgMallocBytes(sizeof(struct free_list), "freeMBlocks");
            new_iter->address = address;
            new_iter->size = size;
            new_iter->next = iter;
            new_iter->prev = iter->prev;
            if (new_iter->prev) {
                new_iter->prev->next = new_iter;
            } else {
                ASSERT(iter == free_list_head);
                free_list_head = new_iter;
            }
            iter->prev = new_iter;
            return;
        }
    }

    /* We're past the last free list entry, so we must
       be the highest allocation so far
    */
    ASSERT(address + size <= mblock_high_watermark);

    /* Fast path the case of releasing high or all memory */
    if (address + size == mblock_high_watermark) {
        mblock_high_watermark -= size;
    } else {
        struct free_list *new_iter;

        new_iter = stgMallocBytes(sizeof(struct free_list), "freeMBlocks");
        new_iter->address = address;
        new_iter->size = size;
        new_iter->next = NULL;
        new_iter->prev = prev;
        if (new_iter->prev) {
            ASSERT(new_iter->prev->next == NULL);
            new_iter->prev->next = new_iter;
        } else {
            ASSERT(free_list_head == NULL);
            free_list_head = new_iter;
        }
    }
}

void releaseFreeMemory(void)
{
    // This function exists for releasing address space
    // on Windows 32 bit
    //
    // Do nothing if USE_LARGE_ADDRESS_SPACE, we never want
    // to release address space

    debugTrace(DEBUG_gc, "mblock_high_watermark: %p\n", mblock_high_watermark);
}

#else // !USE_LARGE_ADDRESS_SPACE

#if SIZEOF_VOID_P == 4
StgWord8 mblock_map[MBLOCK_MAP_SIZE]; // initially all zeros

static void
setHeapAlloced(void *p, StgWord8 i)
{
    mblock_map[MBLOCK_MAP_ENTRY(p)] = i;
}

#elif SIZEOF_VOID_P == 8

MBlockMap **mblock_maps = NULL;

uint32_t mblock_map_count = 0;

MbcCacheLine mblock_cache[MBC_ENTRIES];

static MBlockMap *
findMBlockMap(const void *p)
{
    uint32_t i;
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

StgBool HEAP_ALLOCED_miss(StgWord mblock, const void *p)
{
    MBlockMap *map;
    MBlockMapLine value;
    uint32_t entry_no;

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
        uint32_t entry_no;

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
void * mapEntryToMBlock(uint32_t i)
{
    return (void *)((StgWord)i << MBLOCK_SHIFT);
}

void * getFirstMBlock(void **state STG_UNUSED)
{
    uint32_t i;

    for (i = 0; i < MBLOCK_MAP_SIZE; i++) {
        if (mblock_map[i]) return mapEntryToMBlock(i);
    }
    return NULL;
}

void * getNextMBlock(void **state STG_UNUSED, void *mblock)
{
    uint32_t i;

    for (i = MBLOCK_MAP_ENTRY(mblock) + 1; i < MBLOCK_MAP_SIZE; i++) {
        if (mblock_map[i]) return mapEntryToMBlock(i);
    }
    return NULL;
}

#elif SIZEOF_VOID_P == 8

void * getNextMBlock(void **state STG_UNUSED, void *p)
{
    MBlockMap *map;
    uint32_t off, j;
    uint32_t line_no;
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

void * getFirstMBlock(void **state STG_UNUSED)
{
    MBlockMap *map = mblock_maps[0];
    uint32_t line_no, off;
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

#endif // SIZEOF_VOID_P == 8

static void *getCommittedMBlocks(uint32_t n)
{
    // The OS layer returns committed memory directly
    void *ret = osGetMBlocks(n);
    uint32_t i;

    // fill in the table
    for (i = 0; i < n; i++) {
        markHeapAlloced( (StgWord8*)ret + i * MBLOCK_SIZE );
    }

    return ret;
}

static void decommitMBlocks(void *p, uint32_t n)
{
    osFreeMBlocks(p, n);
    uint32_t i;

    for (i = 0; i < n; i++) {
        markHeapUnalloced( (StgWord8*)p + i * MBLOCK_SIZE );
    }
}

void releaseFreeMemory(void)
{
    osReleaseFreeMemory();
}

#endif /* !USE_LARGE_ADDRESS_SPACE */

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
getMBlocks(uint32_t n)
{
    void *ret;

    ret = getCommittedMBlocks(n);

    debugTrace(DEBUG_gc, "allocated %d megablock(s) at %p",n,ret);

    mblocks_allocated += n;
    peak_mblocks_allocated = stg_max(peak_mblocks_allocated, mblocks_allocated);

    return ret;
}

void *
getMBlocksOnNode(uint32_t node, uint32_t n)
{
    void *addr = getMBlocks(n);
#ifdef DEBUG
    if (RtsFlags.DebugFlags.numa) return addr; // faking NUMA
#endif
    osBindMBlocksToNode(addr, n * MBLOCK_SIZE, numa_map[node]);
    return addr;
}

void *
getMBlockOnNode(uint32_t node)
{
    return getMBlocksOnNode(node, 1);
}

void
freeMBlocks(void *addr, uint32_t n)
{
    debugTrace(DEBUG_gc, "freeing %d megablock(s) at %p",n,addr);

    mblocks_allocated -= n;

    decommitMBlocks(addr, n);
}

void
freeAllMBlocks(void)
{
    debugTrace(DEBUG_gc, "freeing all megablocks");

#ifdef USE_LARGE_ADDRESS_SPACE
    {
        struct free_list *iter, *next;

        for (iter = free_list_head; iter != NULL; iter = next)
        {
            next = iter->next;
            stgFree(iter);
        }
    }

    osReleaseHeapMemory();

    mblock_address_space.begin = (W_)-1;
    mblock_address_space.end = (W_)-1;
    mblock_high_watermark = (W_)-1;
#else
    osFreeAllMBlocks();

#if SIZEOF_VOID_P == 8
    uint32_t n;
    for (n = 0; n < mblock_map_count; n++) {
        stgFree(mblock_maps[n]);
    }
    stgFree(mblock_maps);
#endif

#endif
}

void
initMBlocks(void)
{
    osMemInit();

#ifdef USE_LARGE_ADDRESS_SPACE
    {
        W_ size;
#if aarch64_HOST_ARCH
        size = (W_)1 << 38; // 1/4 TByte
#else
        size = (W_)1 << 40; // 1 TByte
#endif
        void *startAddress = NULL;
        if (RtsFlags.GcFlags.heapBase) {
            startAddress = (void*) RtsFlags.GcFlags.heapBase;
        }
        void *addr = osReserveHeapMemory(startAddress, &size);

        mblock_address_space.begin = (W_)addr;
        mblock_address_space.end = (W_)addr + size;
        mblock_high_watermark = (W_)addr;
    }
#elif SIZEOF_VOID_P == 8
    memset(mblock_cache,0xff,sizeof(mblock_cache));
#endif
}
