/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * External Storage Manger Interface
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTS_STORAGE_GC_H
#define RTS_STORAGE_GC_H

#include <stddef.h>
#include "rts/OSThreads.h"

/* -----------------------------------------------------------------------------
 * Generational GC
 *
 * We support an arbitrary number of generations, with an arbitrary number
 * of steps per generation.  Notes (in no particular order):
 *
 *       - all generations except the oldest should have the same
 *         number of steps.  Multiple steps gives objects a decent
 *         chance to age before being promoted, and helps ensure that
 *         we don't end up with too many thunks being updated in older
 *         generations.
 *
 *       - the oldest generation has one step.  There's no point in aging
 *         objects in the oldest generation.
 *
 *       - generation 0, step 0 (G0S0) is the allocation area.  It is given
 *         a fixed set of blocks during initialisation, and these blocks
 *         normally stay in G0S0.  In parallel execution, each
 *         Capability has its own nursery.
 *
 *       - during garbage collection, each step which is an evacuation
 *         destination (i.e. all steps except G0S0) is allocated a to-space.
 *         evacuated objects are allocated into the step's to-space until
 *         GC is finished, when the original step's contents may be freed
 *         and replaced by the to-space.
 *
 *       - the mutable-list is per-generation (not per-step).  G0 doesn't 
 *         have one (since every garbage collection collects at least G0).
 * 
 *       - block descriptors contain pointers to both the step and the
 *         generation that the block belongs to, for convenience.
 *
 *       - static objects are stored in per-generation lists.  See GC.c for
 *         details of how we collect CAFs in the generational scheme.
 *
 *       - large objects are per-step, and are promoted in the same way
 *         as small objects, except that we may allocate large objects into
 *         generation 1 initially.
 *
 * ------------------------------------------------------------------------- */

// A count of blocks needs to store anything up to the size of memory
// divided by the block size.  The safest thing is therefore to use a
// type that can store the full range of memory addresses,
// ie. StgWord.  Note that we have had some tricky int overflows in a
// couple of cases caused by using ints rather than longs (e.g. #5086)

typedef StgWord memcount;

typedef struct nursery_ {
    bdescr *       blocks;
    memcount       n_blocks;
} nursery;

typedef struct generation_ {
    nat            no;			// generation number

    bdescr *       blocks;	        // blocks in this gen
    memcount       n_blocks;            // number of blocks
    memcount       n_words;             // number of used words

    bdescr *       large_objects;	// large objects (doubly linked)
    memcount       n_large_blocks;      // no. of blocks used by large objs
    memcount       n_large_words;       // no. of words used by large objs
    memcount       n_new_large_words;   // words of new large objects
                                        // (for doYouWantToGC())

    memcount       max_blocks;          // max blocks

    StgTSO *       threads;             // threads in this gen
                                        // linked via global_link
    StgWeak *      weak_ptr_list;       // weak pointers in this gen

    struct generation_ *to;		// destination gen for live objects

    // stats information
    nat collections;
    nat par_collections;
    nat failed_promotions;

    // ------------------------------------
    // Fields below are used during GC only

#if defined(THREADED_RTS)
    char pad[128];                      // make sure the following is
                                        // on a separate cache line.
    SpinLock     sync;                  // lock for large_objects
                                        //    and scavenged_large_objects
#endif

    int          mark;			// mark (not copy)? (old gen only)
    int          compact;		// compact (not sweep)? (old gen only)

    // During GC, if we are collecting this gen, blocks and n_blocks
    // are copied into the following two fields.  After GC, these blocks
    // are freed.
    bdescr *     old_blocks;	        // bdescr of first from-space block
    memcount     n_old_blocks;         // number of blocks in from-space
    memcount     live_estimate;         // for sweeping: estimate of live data
    
    bdescr *     scavenged_large_objects;  // live large objs after GC (d-link)
    memcount     n_scavenged_large_blocks; // size (not count) of above

    bdescr *     bitmap;  		// bitmap for compacting collection

    StgTSO *     old_threads;
    StgWeak *    old_weak_ptr_list;
} generation;

extern generation * generations;
extern generation * g0;
extern generation * oldest_gen;

/* -----------------------------------------------------------------------------
   Generic allocation

   StgPtr allocate(Capability *cap, W_ n)
                                Allocates memory from the nursery in
                                the current Capability. This can be
                                done without taking a global lock,
                                unlike allocate(). In the event of a
                                heap overflow the program will be
                                terminated.

   StgPtr allocateFail(Capability *cap, W_ n)
                                Similar to allocate() but returns NULL
                                in the event of a heap overflow.

   StgPtr allocatePinned(Capability *cap, W_ n)
                                Allocates a chunk of contiguous store
   				n words long, which is at a fixed
				address (won't be moved by GC).  
				Returns a pointer to the first word.
				Always succeeds.
				
				NOTE: the GC can't in general handle
				pinned objects, so allocatePinned()
				can only be used for ByteArrays at the
				moment.

				Don't forget to TICK_ALLOC_XXX(...)
				after calling allocate or
				allocatePinned, for the
				benefit of the ticky-ticky profiler.

   -------------------------------------------------------------------------- */

StgPtr  allocate        ( Capability *cap, W_ n );
StgPtr  allocateFail    ( Capability *cap, W_ n );
StgPtr  allocatePinned  ( Capability *cap, W_ n );

/* memory allocator for executable memory */
typedef void* AdjustorWritable;
typedef void* AdjustorExecutable;

AdjustorWritable allocateExec(W_ len, AdjustorExecutable *exec_addr);
void             freeExec (AdjustorExecutable p);

// Used by GC checks in external .cmm code:
extern W_ large_alloc_lim;

/* -----------------------------------------------------------------------------
   Performing Garbage Collection
   -------------------------------------------------------------------------- */

void performGC(void);
void performMajorGC(void);

/* -----------------------------------------------------------------------------
   The CAF table - used to let us revert CAFs in GHCi
   -------------------------------------------------------------------------- */

StgWord newCAF    (StgRegTable *reg, StgClosure *caf, StgClosure *bh);
StgWord newDynCAF (StgRegTable *reg, StgClosure *caf, StgClosure *bh);
void revertCAFs (void);

// Request that all CAFs are retained indefinitely.
void setKeepCAFs (void);

/* -----------------------------------------------------------------------------
   Stats
   -------------------------------------------------------------------------- */

typedef struct _GCStats {
  StgWord64 bytes_allocated;
  StgWord64 num_gcs;
  StgWord64 num_byte_usage_samples;
  StgWord64 max_bytes_used;
  StgWord64 cumulative_bytes_used;
  StgWord64 bytes_copied;
  StgWord64 current_bytes_used;
  StgWord64 current_bytes_slop;
  StgWord64 max_bytes_slop;
  StgWord64 peak_megabytes_allocated;
  StgWord64 par_tot_bytes_copied;
  StgWord64 par_max_bytes_copied;
  StgDouble mutator_cpu_seconds;
  StgDouble mutator_wall_seconds;
  StgDouble gc_cpu_seconds;
  StgDouble gc_wall_seconds;
  StgDouble cpu_seconds;
  StgDouble wall_seconds;
} GCStats;
void getGCStats (GCStats *s);
rtsBool getGCStatsEnabled (void);

// These don't change over execution, so do them elsewhere
//  StgDouble init_cpu_seconds;
//  StgDouble init_wall_seconds;

typedef struct _ParGCStats {
  StgWord64 tot_copied;
  StgWord64 max_copied;
} ParGCStats;
void getParGCStats (ParGCStats *s);

/*
typedef struct _TaskStats {
  StgWord64 mut_time;
  StgWord64 mut_etime;
  StgWord64 gc_time;
  StgWord64 gc_etime;
} TaskStats;
// would need to allocate arbitrarily large amount of memory
// because it's a linked list of results
void getTaskStats (TaskStats **s);
// Need to stuff SparkCounters in a public header file...
void getSparkStats (SparkCounters *s);
*/

// Returns the total number of bytes allocated since the start of the program.
HsInt64 getAllocations (void);

/* -----------------------------------------------------------------------------
   This is the write barrier for MUT_VARs, a.k.a. IORefs.  A
   MUT_VAR_CLEAN object is not on the mutable list; a MUT_VAR_DIRTY
   is.  When written to, a MUT_VAR_CLEAN turns into a MUT_VAR_DIRTY
   and is put on the mutable list.
   -------------------------------------------------------------------------- */

void dirty_MUT_VAR(StgRegTable *reg, StgClosure *p);

/* set to disable CAF garbage collection in GHCi. */
/* (needed when dynamic libraries are used). */
extern rtsBool keepCAFs;

INLINE_HEADER void initBdescr(bdescr *bd, generation *gen, generation *dest)
{
    bd->gen     = gen;
    bd->gen_no  = gen->no;
    bd->dest_no = dest->no;
}

#endif /* RTS_STORAGE_GC_H */
