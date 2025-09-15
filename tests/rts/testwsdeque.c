#define THREADED_RTS

#include "Rts.h"
#include "WSDeque.h"
#include <stdio.h>

#define SCRATCH_SIZE (1024*1024)
#define THREADS 3
#define POP 2

WSDeque *q;

StgWord scratch[SCRATCH_SIZE];
StgWord done;

OSThreadId ids[THREADS];

// -----------------------------------------------------------------------------
// version of stealWSDeque() that logs its actions, for debugging

#if defined(DEBUG)

#define BUF 128

int bufs[THREADS];

StgWord last_b[THREADS][BUF];
StgWord last_t[THREADS][BUF];
StgWord last_v[THREADS][BUF];

#define CASTOP(addr,old,new) ((old) == cas(((StgPtr)addr),(old),(new)))

void *
myStealWSDeque_ (WSDeque *q, uint32_t n)
{
    void * stolen;

// Can't do this on someone else's spark pool:
// ASSERT_WSDEQUE_INVARIANTS(q);

    // NB. these loads must be ordered, otherwise there is a race
    // between steal and pop.
    StgWord t = ACQUIRE_LOAD(&q->top);
    SEQ_CST_FENCE();
    StgWord b = ACQUIRE_LOAD(&q->bottom);

    void *result = NULL;
    if (t < b) {
        /* Non-empty queue */
        result = RELAXED_LOAD(&q->elements[t % q->size]);
        if (!cas_top(q, t, t+1)) {
            return NULL;
        }
    }
    return result;
}

void *
myStealWSDeque (WSDeque *q, uint32_t n)
{
    void *stolen;

    do {
        stolen = myStealWSDeque_(q,n);
    } while (stolen == NULL && !looksEmptyWSDeque(q));

    return stolen;
}

void dump(void)
{
    uint32_t n;
    uint32_t i;
    for (n = 0; n < THREADS; n++) {
        debugBelch("\nthread %d:\n", n);
        for (i = bufs[n]; i >= stg_max(bufs[n]-20,0); i--) {
            debugBelch("%d: t=%ld b=%ld = %ld\n", i, last_t[n][i], last_b[n][i], last_v[n][i]);
        }
    }
}

#endif // DEBUG

// -----------------------------------------------------------------------------

void work(void *p, uint32_t n)
{
    StgWord val;

    // debugBelch("work %ld %d\n", p, n);
    val = *(StgWord *)p;
    if (val != 0) {
        fflush(stdout);
        fflush(stderr);
        barf("FAIL: %p %" FMT_Word32 " %" FMT_Word, p, n, val);
    }
    *(StgWord*)p = n+10;
}

void* OSThreadProcAttr thief(void *info)
{
    void *p;
    StgWord n;
    uint32_t count = 0;

    n = (StgWord)info;

    while (!done) {
#if defined(DEBUG)
        p = myStealWSDeque(q,n);
#else
        p = stealWSDeque(q);
#endif
        if (p != NULL) { work(p,n+1); count++; }
    }
    debugBelch("thread %ld finished, stole %d", n, count);
    return NULL;
}

int main(int argc, char*argv[])
{
    int n;
    uint32_t count = 0;
    void *p;

    q = newWSDeque(1024);
    done = 0;

    for (n=0; n < SCRATCH_SIZE; n++) {
        scratch[n] = 0;
    }

    for (n=0; n < THREADS; n++) {
        createOSThread(&ids[n], "thief", (OSThreadProc*)thief, (void*)(StgWord)n);
    }

    for (n=0; n < SCRATCH_SIZE; n++) {
        if (n % POP) {
            p = popWSDeque(q);
            if (p != NULL) { work(p,0); count++; }
        }
        pushWSDeque(q,&scratch[n]);
    }

#if defined(DEBUG)
    debugBelch("main thread finished, popped %d", count);
#endif
    exit(0);
}
