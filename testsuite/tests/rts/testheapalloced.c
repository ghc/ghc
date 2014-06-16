#include "Rts.h"
#include "RtsFlags.h"
#ifdef DEBUG
#define INLINE_HEADER
#endif
#include "MBlock.h"
#ifdef DEBUG
extern void *getFirstMBlock(void);
extern void *getNextMBlock(void *mblock);
#endif

#include <stdio.h>
#include <string.h>

extern bdescr *allocGroup_lock_lock(nat n);
extern void freeGroup_lock(bdescr *p);

const int ARRSIZE  = 2000;
const int LOOPS    = 20000;
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

    memset(a, 0, ARRSIZE * sizeof(bdescr*));

    for (i=0; i < LOOPS; i++)
    {
        j = rand() % ARRSIZE;
        if (a[j]) { freeGroup_lock(a[j]); }
        a[j] = allocGroup_lock(rand() % MAXALLOC + 1);
    }

#ifdef DEBUG
    {
        void *p;
        i = 0;
        for (p = getFirstMBlock(); p != NULL; p = getNextMBlock(p))
        {
            if (!HEAP_ALLOCED(p)) barf("%p",p);
            i++;
        }
        printf("%d\n", i);
    }
#endif

    {
        void *p, *base;

        j = 0;
        base = RtsFlags.GcFlags.heapBase;
        
        for (i=0; i < LOOPS*2000; i++)
        {
            // this is for testing: generate random addresses anywhere
            // in the address space.
            //
            // 48 bits is: 0x800000000000 - 0x7fffffffffff
            // so ((StgInt)rand() >> 4) varies between -2^27 and 2^27-1.
            // and << 20 of this is a random signed 48-bit megablock address
            //
            // p = (void*)((StgWord)((StgInt)rand() >> 4) << 20);

            // this is for benchmarking: roughly half of these
            // addresses will be in the heap.
            p = base + (((StgWord)rand() << 10) % 
                        ((StgWord)ARRSIZE * MAXALLOC * BLOCK_SIZE));

            if (HEAP_ALLOCED(p)) {
                // printf("%p\n",p);
                j++;
            }
        }
        printf("%d\n", j);
    }

    printf("misses: %ld, %ld%\n", mpc_misses, mpc_misses / (LOOPS*20));

    for (i=0; i < ARRSIZE; i++)
    {
        if (a[i]) { freeGroup_lock(a[i]); }
    }

    hs_exit(); // will do a memory leak test

    exit(0);
}

// 48 bits is: 0x800000000000 - 0x7fffffffffff
