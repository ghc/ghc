/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2019
 *
 * Internal definitions for subordinate heap profilers to consume
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "Hash.h"
#include "Arena.h"

#include "BeginPrivate.h"

/* -----------------------------------------------------------------------------
 * Counters
 *
 * For most heap profiles each closure identity gets a simple count
 * of live words in the heap at each census.  However, if we're
 * selecting by biography, then we have to keep the various
 * lag/drag/void counters for each identity.
 * -------------------------------------------------------------------------- */
typedef struct _counter {
    const void *identity;
    union {
        ssize_t resid;
        struct {
            // Total sizes of:
            ssize_t prim;     // 'inherently used' closures
            ssize_t not_used; // 'never used' closures
            ssize_t used;     // 'used at least once' closures
            ssize_t void_total;  // 'destroyed without being used' closures
            ssize_t drag_total;  // 'used at least once and waiting to die'
        } ldv;
    } c;
    struct _counter *next;
} counter;

typedef struct {
    double      time;    // the time in MUT time when the census is made
    StgWord64   rtime;   // The eventlog time the census was made. This is used
                         // for the LDV profiling events because they are all
                         // emitted at the end of compilation so we need to know
                         // when the sample actually took place.
    HashTable * hash;
    counter   * ctrs;
    Arena     * arena;

    // for LDV profiling, when just displaying by LDV
    ssize_t    prim;
    ssize_t    not_used;
    ssize_t    used;
    ssize_t    void_total;
    ssize_t    drag_total;
} Census;

void initLDVCtr(counter *ctr);
counter* heapInsertNewCounter(Census *census, StgWord identity);

#include "EndPrivate.h"
