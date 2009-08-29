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

typedef struct step_ {
    unsigned int         no;		// step number in this generation
    unsigned int         abs_no;	// absolute step number

    struct generation_ * gen;		// generation this step belongs to
    unsigned int         gen_no;        // generation number (cached)

    bdescr *             blocks;	// blocks in this step
    unsigned int         n_blocks;	// number of blocks
    unsigned int         n_words;       // number of words

    struct step_ *       to;		// destination step for live objects

    bdescr *             large_objects;	 // large objects (doubly linked)
    unsigned int         n_large_blocks; // no. of blocks used by large objs

    StgTSO *             threads;       // threads in this step
                                        // linked via global_link

    // ------------------------------------
    // Fields below are used during GC only

    // During GC, if we are collecting this step, blocks and n_blocks
    // are copied into the following two fields.  After GC, these blocks
    // are freed.

#if defined(THREADED_RTS)
    char pad[128];                      // make sure the following is
                                        // on a separate cache line.
    SpinLock     sync_large_objects;    // lock for large_objects
                                        //    and scavenged_large_objects
#endif

    int          mark;			// mark (not copy)? (old gen only)
    int          compact;		// compact (not sweep)? (old gen only)

    bdescr *     old_blocks;	        // bdescr of first from-space block
    unsigned int n_old_blocks;		// number of blocks in from-space
    unsigned int live_estimate;         // for sweeping: estimate of live data
    
    bdescr *     part_blocks;           // partially-full scanned blocks
    unsigned int n_part_blocks;         // count of above

    bdescr *     scavenged_large_objects;  // live large objs after GC (d-link)
    unsigned int n_scavenged_large_blocks; // size (not count) of above

    bdescr *     bitmap;  		// bitmap for compacting collection

    StgTSO *     old_threads;

} step;


typedef struct generation_ {
    unsigned int   no;			// generation number
    step *         steps;		// steps
    unsigned int   n_steps;		// number of steps
    unsigned int   max_blocks;		// max blocks in step 0
    bdescr        *mut_list;      	// mut objects in this gen (not G0)
    
    // stats information
    unsigned int collections;
    unsigned int par_collections;
    unsigned int failed_promotions;

    // temporary use during GC:
    bdescr        *saved_mut_list;
} generation;

extern generation * generations;

extern generation * g0;
extern step * g0s0;
extern generation * oldest_gen;
extern step * all_steps;
extern nat total_steps;

/* -----------------------------------------------------------------------------
   Generic allocation

   StgPtr allocateInGen(generation *g, nat n)
                                Allocates a chunk of contiguous store
   				n words long in generation g,
   				returning a pointer to the first word.
   				Always succeeds.
				
   StgPtr allocate(nat n)       Equaivalent to allocateInGen(g0)
				
   StgPtr allocateLocal(Capability *cap, nat n)
                                Allocates memory from the nursery in
				the current Capability.  This can be
				done without taking a global lock,
                                unlike allocate().

   StgPtr allocatePinned(nat n) Allocates a chunk of contiguous store
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

   rtsBool doYouWantToGC(void)  Returns True if the storage manager is
   				ready to perform a GC, False otherwise.

   lnat  allocatedBytes(void)  Returns the number of bytes allocated
                                via allocate() since the last GC.
				Used in the reporting of statistics.

   -------------------------------------------------------------------------- */

StgPtr  allocate        ( lnat n );
StgPtr  allocateInGen   ( generation *g, lnat n );
StgPtr  allocateLocal   ( Capability *cap, lnat n );
StgPtr  allocatePinned  ( lnat n );
lnat    allocatedBytes  ( void );

/* memory allocator for executable memory */
void * allocateExec(unsigned int len, void **exec_addr);
void   freeExec (void *p);

// Used by GC checks in external .cmm code:
extern nat alloc_blocks;
extern nat alloc_blocks_lim;

/* -----------------------------------------------------------------------------
   Performing Garbage Collection
   -------------------------------------------------------------------------- */

void performGC(void);
void performMajorGC(void);

/* -----------------------------------------------------------------------------
   The CAF table - used to let us revert CAFs in GHCi
   -------------------------------------------------------------------------- */

void newCAF     (StgClosure*);
void newDynCAF  (StgClosure *);
void revertCAFs (void);

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

#endif /* RTS_STORAGE_GC_H */
