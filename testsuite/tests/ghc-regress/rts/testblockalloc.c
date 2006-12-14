#include "Rts.h"
#include "RtsFlags.h"

#include <stdio.h>

extern bdescr *allocGroup_lock_lock(nat n);
extern void freeGroup_lock(bdescr *p);

const int ARRSIZE  = 256;
const int LOOPS    = 100;
const int MAXALLOC = ((8 * 1024 * 1024) / BLOCK_SIZE - 1);
//const int MAXALLOC = ((4 * 1024 * 1024) / BLOCK_SIZE - 1);
const int SEED     = 0xf00f00;

extern lnat mblocks_allocated;

int main (int argc, char *argv[])
{
    int i, j, b;

    bdescr *a[ARRSIZE];

    srand(SEED);

    hs_init(&argc, &argv);

    // repeatedly sweep though the array, allocating new random-sized
    // objects and deallocating the old ones.
    for (i=0; i < LOOPS; i++)
    {
        for (j=0; j < ARRSIZE; j++)
        {
            if (i > 0)
            {
                freeGroup_lock(a[j]);
                DEBUG_ONLY(checkFreeListSanity());
            }
            a[j] = allocGroup_lock((rand() % MAXALLOC) + 1);
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
            a[j] = allocGroup_lock((rand() % MAXALLOC) + 1);
            DEBUG_ONLY(checkFreeListSanity());
        }
        for (j=ARRSIZE-1; j >= 0; j--)
        {
            freeGroup_lock(a[j]);
            DEBUG_ONLY(checkFreeListSanity());
        }
    }
    
    DEBUG_ONLY(checkFreeListSanity());

    hs_exit(); // will do a memory leak test

    exit(0);
}
