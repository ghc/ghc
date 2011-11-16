#include "Rts.h"

#include <stdio.h>

extern bdescr *allocGroup_lock_lock(nat n);
extern void freeGroup_lock(bdescr *p);

const int ARRSIZE  = 256;
const int LOOPS    = 100;
const int MAXALLOC = ((8 * 1024 * 1024) / BLOCK_SIZE - 1);
//const int MAXALLOC = ((64 * 1024 * 1024) / BLOCK_SIZE - 1);
const int SEED     = 0xf00f00;

extern lnat mblocks_allocated;

int main (int argc, char *argv[])
{
    int i, j, b;

    bdescr *a[ARRSIZE];

    srand(SEED);

#if __GLASGOW_HASKELL__ >= 703
    {
        RtsConfig conf = defaultRtsConfig;
        conf.rts_opts_enabled = RtsOptsAll;
        hs_init_ghc(&argc, &argv, conf);
    }
#else
    hs_init(&argc, &argv);
#endif

   // repeatedly sweep though the array, allocating new random-sized
   // objects and deallocating the old ones.
   for (i=0; i < LOOPS; i++)
   {
       for (j=0; j < ARRSIZE; j++)
       {
           if (i > 0)
           {
               IF_DEBUG(block_alloc, debugBelch("A%d: freeing %p, %d blocks @ %p\n", j, a[j], a[j]->blocks, a[j]->start));
               freeGroup_lock(a[j]);
               DEBUG_ONLY(checkFreeListSanity());
           }
           b = (rand() % MAXALLOC) + 1;
           a[j] = allocGroup_lock(b);
           IF_DEBUG(block_alloc, debugBelch("A%d: allocated %p, %d blocks @ %p\n", j, a[j], b, a[j]->start));
           // allocating zero blocks isn't allowed
           DEBUG_ONLY(checkFreeListSanity());
       }
   }

   for (j=0; j < ARRSIZE; j++)
   {
       freeGroup_lock(a[j]);
   }
    
    // this time, sweep forwards allocating new blocks, and then
    // backwards deallocating them.
    for (i=0; i < LOOPS; i++)
    {
        for (j=0; j < ARRSIZE; j++)
        {
            b = (rand() % MAXALLOC) + 1;
            a[j] = allocGroup_lock(b);
            IF_DEBUG(block_alloc, debugBelch("B%d,%d: allocated %p, %d blocks @ %p\n", i, j, a[j], b, a[j]->start));
            DEBUG_ONLY(checkFreeListSanity());
        }
        for (j=ARRSIZE-1; j >= 0; j--)
        {
            IF_DEBUG(block_alloc, debugBelch("B%d,%d: freeing %p, %d blocks @ %p\n", i, j, a[j], a[j]->blocks, a[j]->start));
            freeGroup_lock(a[j]);
            DEBUG_ONLY(checkFreeListSanity());
        }
    }
    
    DEBUG_ONLY(checkFreeListSanity());

    hs_exit(); // will do a memory leak test

    exit(0);
}
