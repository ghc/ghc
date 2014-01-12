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

#ifdef DEBUG

#define BUF 128

int bufs[THREADS];

StgWord last_b[THREADS][BUF];
StgWord last_t[THREADS][BUF];
StgWord last_v[THREADS][BUF];

#define CASTOP(addr,old,new) ((old) == cas(((StgPtr)addr),(old),(new)))

void *
myStealWSDeque_ (WSDeque *q, nat n)
{
    void * stolen;
    StgWord b,t; 
    
// Can't do this on someone else's spark pool:
// ASSERT_WSDEQUE_INVARIANTS(q); 
    
    // NB. these loads must be ordered, otherwise there is a race
    // between steal and pop.
    t = q->top;
    load_load_barrier();
    b = q->bottom;
    
    // NB. b and t are unsigned; we need a signed value for the test
    // below, because it is possible that t > b during a
    // concurrent popWSQueue() operation.
    if ((long)b - (long)t <= 0 ) { 
        return NULL; /* already looks empty, abort */
  }
    
    /* now access array, see pushBottom() */
    stolen = q->elements[t & q->moduloSize];
    
    /* now decide whether we have won */
    if ( !(CASTOP(&(q->top),t,t+1)) ) {
        /* lost the race, someon else has changed top in the meantime */
        return NULL;
    }  /* else: OK, top has been incremented by the cas call */

    // debugBelch("stealWSDeque_: t=%d b=%d\n", t, b);

// Can't do this on someone else's spark pool:
// ASSERT_WSDEQUE_INVARIANTS(q); 

    bufs[n] ++;
    if (bufs[n] == BUF) { bufs[n] = 0; }
    last_b[n][bufs[n]] = b;
    last_t[n][bufs[n]] = t;
    last_v[n][bufs[n]] = (StgWord)stolen;
    return stolen;
}

void *
myStealWSDeque (WSDeque *q, nat n)
{
    void *stolen;
    
    do { 
        stolen = myStealWSDeque_(q,n);
    } while (stolen == NULL && !looksEmptyWSDeque(q));
    
    return stolen;
}

void dump(void)
{
    nat n;
    nat i;
    for (n = 0; n < THREADS; n++) {
        debugBelch("\nthread %d:\n", n);
        for (i = bufs[n]; i >= stg_max(bufs[n]-20,0); i--) {
            debugBelch("%d: t=%ld b=%ld = %ld\n", i, last_t[n][i], last_b[n][i], last_v[n][i]);
        }
    }
}

#endif // DEBUG

// -----------------------------------------------------------------------------

void work(void *p, nat n)
{
    StgWord val;

    // debugBelch("work %ld %d\n", p, n);
    val = *(StgWord *)p;
    if (val != 0) { 
        fflush(stdout); 
        fflush(stderr); 
        barf("FAIL: %ld %d %d", p, n, val);
    }
    *(StgWord*)p = n+10;
}
    
void OSThreadProcAttr thief(void *info)
{
    void *p;
    StgWord n;
    nat count = 0;

    n = (StgWord)info;

    while (!done) {
#ifdef DEBUG
        p = myStealWSDeque(q,n);
#else
        p = stealWSDeque(q);
#endif
        if (p != NULL) { work(p,n+1); count++; }
    }
    debugBelch("thread %ld finished, stole %d", n, count);
}

int main(int argc, char*argv[])
{
    int n;
    nat count = 0;
    void *p;

    q = newWSDeque(1024);
    done = 0;
    
    for (n=0; n < SCRATCH_SIZE; n++) {
        scratch[n] = 0;
    }

    for (n=0; n < THREADS; n++) {
        createOSThread(&ids[n], thief, (void*)(StgWord)n);
    }

    for (n=0; n < SCRATCH_SIZE; n++) {
        if (n % POP) {
            p = popWSDeque(q);
            if (p != NULL) { work(p,0); count++; }
        }
        pushWSDeque(q,&scratch[n]);
    }

#ifdef DEBUG
    debugBelch("main thread finished, popped %d", count);
#endif
    exit(0);
}
