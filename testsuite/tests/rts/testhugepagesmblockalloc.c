#include "Rts.h"

#include <stdio.h>

// 16 * 64 == max 1GB
const int MAXALLOC = 16;
const int ARRSIZE  = 64;

const int LOOPS    = 1000;
const int SEED     = 0xf00f00;

extern StgWord mblocks_allocated;

int main (int argc, char *argv[])
{
    int i, j, b;

    void *a[ARRSIZE];
    uint32_t sizes[ARRSIZE];

    srand(SEED);

    {
        RtsConfig conf = defaultRtsConfig;
        conf.rts_opts_enabled = RtsOptsAll;
        hs_init_ghc(&argc, &argv, conf);
    }

   // repeatedly sweep though the array, allocating new random-sized
   // objects and deallocating the old ones.
   for (i=0; i < LOOPS; i++)
   {
       for (j=0; j < ARRSIZE; j++)
       {
           if (i > 0)
           {
               freeMBlocks(a[j], sizes[j]);
           }
           b = (rand() % MAXALLOC) + 1;
           a[j] = getMBlocks(b);
           sizes[j] = b;
       }
   }

   releaseFreeMemory();

   for (j=0; j < ARRSIZE; j++)
   {
       freeMBlocks(a[j], sizes[j]);
   }

   releaseFreeMemory();

    // this time, sweep forwards allocating new blocks, and then
    // backwards deallocating them.
    for (i=0; i < LOOPS; i++)
    {
        for (j=0; j < ARRSIZE; j++)
        {
            b = (rand() % MAXALLOC) + 1;
            a[j] = getMBlocks(b);
            sizes[j] = b;
        }
        for (j=ARRSIZE-1; j >= 0; j--)
        {
            freeMBlocks(a[j], sizes[j]);
        }
    }

    releaseFreeMemory();

    hs_exit(); // will do a memory leak test

    exit(0);
}
