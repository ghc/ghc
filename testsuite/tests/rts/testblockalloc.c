#include "Rts.h"

#include <stdio.h>

extern bdescr *allocGroup_lock_lock(uint32_t n);
extern bdescr *allocAlignedGroupOnNode (uint32_t node, W_ n);
extern void freeGroup_lock(bdescr *p);

const int ARRSIZE  = 256;
const int LOOPS    = 100;
const int MAXALLOC = ((8 * 1024 * 1024) / BLOCK_SIZE - 1);
//const int MAXALLOC = ((64 * 1024 * 1024) / BLOCK_SIZE - 1);
const int SEED     = 0xf00f00;

extern StgWord mblocks_allocated;

static void test_random_alloc(void)
{
    bdescr *a[ARRSIZE];

    // repeatedly sweep though the array, allocating new random-sized
    // objects and deallocating the old ones.
    for (int i=0; i < LOOPS; i++)
    {
        for (int j=0; j < ARRSIZE; j++)
        {
            if (i > 0)
            {
                IF_DEBUG(block_alloc, debugBelch("A%d: freeing %p, %d blocks @ %p\n", j, a[j], a[j]->blocks, bdescr_start(a[j])));
                freeGroup_lock(a[j]);
                DEBUG_ONLY(checkFreeListSanity());
            }

            int b = (rand() % MAXALLOC) + 1;
            a[j] = allocGroup_lock(b);
            IF_DEBUG(block_alloc, debugBelch("A%d: allocated %p, %d blocks @ %p\n", j, a[j], b, bdescr_start(a[j])));
            // allocating zero blocks isn't allowed
            DEBUG_ONLY(checkFreeListSanity());
        }
    }

    for (int j=0; j < ARRSIZE; j++)
    {
        freeGroup_lock(a[j]);
    }
}

static void test_sequential_alloc(void)
{
    bdescr *a[ARRSIZE];

    // this time, sweep forwards allocating new blocks, and then
    // backwards deallocating them.
    for (int i=0; i < LOOPS; i++)
    {
        for (int j=0; j < ARRSIZE; j++)
        {
            int b = (rand() % MAXALLOC) + 1;
            a[j] = allocGroup_lock(b);
            IF_DEBUG(block_alloc, debugBelch("B%d,%d: allocated %p, %d blocks @ %p\n", i, j, a[j], b, bdescr_start(a[j])));
            DEBUG_ONLY(checkFreeListSanity());
        }
        for (int j=ARRSIZE-1; j >= 0; j--)
        {
            IF_DEBUG(block_alloc, debugBelch("B%d,%d: freeing %p, %d blocks @ %p\n", i, j, a[j], a[j]->blocks, bdescr_start(a[j])));
            freeGroup_lock(a[j]);
            DEBUG_ONLY(checkFreeListSanity());
        }
    }
}

static void test_aligned_alloc(void)
{
    bdescr *a[ARRSIZE];

    // this time, sweep forwards allocating new blocks, and then
    // backwards deallocating them.
    for (int i=0; i < LOOPS; i++)
    {
        for (int j=0; j < ARRSIZE; j++)
        {
            // allocAlignedGroupOnNode does not support allocating more than
            // BLOCKS_PER_MBLOCK/2 blocks.
            int b = rand() % (BLOCKS_PER_MBLOCK / 2);
            if (b == 0) { b = 1; }
            a[j] = allocAlignedGroupOnNode(0, b);
            if ((((W_)(bdescr_start(a[j]))) % (b*BLOCK_SIZE)) != 0)
            {
                barf("%p is not aligned to allocation size %d", a[j], b);
            }
            IF_DEBUG(block_alloc, debugBelch("B%d,%d: allocated %p, %d blocks @ %p\n", i, j, a[j], b, bdescr_start(a[j])));
            DEBUG_ONLY(checkFreeListSanity());
        }
        for (int j=ARRSIZE-1; j >= 0; j--)
        {
            IF_DEBUG(block_alloc, debugBelch("B%d,%d: freeing %p, %d blocks @ %p\n", i, j, a[j], a[j]->blocks, bdescr_start(a[j])));
            freeGroup_lock(a[j]);
            DEBUG_ONLY(checkFreeListSanity());
        }
    }
}

int main (int argc, char *argv[])
{
    int i, j, b;

    bdescr *a[ARRSIZE];

    srand(SEED);

    {
        RtsConfig conf = defaultRtsConfig;
        conf.rts_opts_enabled = RtsOptsAll;
        hs_init_ghc(&argc, &argv, conf);
    }

    test_random_alloc();
    test_sequential_alloc();
    test_aligned_alloc();

    DEBUG_ONLY(checkFreeListSanity());

    hs_exit(); // will do a memory leak test

    exit(0);
}
