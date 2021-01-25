/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2020
 *
 * Prototypes for functions in ClosureTable.c
 *
 * A table of pointers to closures on the GC heap with stable indexes.
 *
 * It provides O(1) alloc, free and lookup. The table can be expanded using a
 * simple doubling strategy: in which case allocation is typically O(1) and
 * occasionally O(n) for overall amortised O(1). No shrinking is used.
 *
 * The table is itself heap allocated, and points to other heap objects.
 * As such, in most expected use cases it is necessary mark the table as a GC
 * root to keep the table entries alive, and maintain proper pointers to them
 * as the GC moves heap objects about. It can be freed simply by forgetting the
 * pointer to the table and not marking it as a GC root in the next GC.
 *
 * It is designed to be allocated and accessed exclusively from a single
 * capability, enabling it to work without any locking.
 *
 * This table is used in some of the I/O managers to keep track of in-flight
 * I/O operations (but not timers). This allows the tracking info to be kept
 * on the (unpinned) GC heap, and shared with Haskell code, and by putting a
 * pointer to the tracking information in a table, the index remains stable and
 * can be passed via foreign code (like the kernel).
 *
 * It is thus similar to the StablePtr table, but per-capability which removes
 * the need for locking. It _should_ also provide lower GC pause times with the
 * non-moving GC by spending only O(1) time to mark as a GC root, vs O(n) for
 * markStablePtrTable.
 *
 * -------------------------------------------------------------------------*/

#pragma once

#include "Capability.h"

#include "BeginPrivate.h"


/* The ClosureTable can be used in two different modes:
 *
 * + not compact mode: stable entry indexes and sparse entry indexes
 * + compact mode: non-stable entry indexes and dense entry indexes (this means
 *   all table entries are in the indexes [0 .. size-1] with no gaps).
 *
 * The mode of use is declared when the table is created and the use of the
 * table must be consistent with this compacting mode:
 *
 * + For non-compact use, only use removeClosureTable.
 * + For compact use, only use removeCompactClosureTable and only use
 *   enlargeClosureTable when the table is already full (which would be typical
 *   use anyway).
 *
 * Many closure table use cases rely on the stability of the index of entries
 * added to the table (e.g. to use as a kind of stable pointer). Such use cases
 * are obviously incompatible with compacting. In this case gaps in the table
 * are inevitable, and iterating over all table entries is either inefficient
 * (when the table is mostly empty) or needs auxiliary storage to track.
 *
 * In some other use cases the stable entry indexes are unnecessary, but
 * iterating over all the table entries efficiently is important. For such use
 * cases it makes sense to use keep the table compact.
 */
enum ClosureTableMode {
    ClosureTableNonCompact = 0,
    ClosureTableCompact    = 1
};


typedef struct {
    /* An array of heap-allocated closures. The array itself is allocated
     * on the GC heap. It is a GC root.
     */
    StgMutArrPtrs *arr;

    /* An array used as a free list. It is allocated on the GC heap. It is a
     * GC root. We treat the payload as an array of C ints.
     */
    StgArrBytes *free;

    /* The head pointer in the free list */
    int free_hd;

    /* The number of used elements in the table */
    int elems;

    /* The length of each array (in elements, not bytes) */
    int capacity;

    /* For sanity checking only: is the table being kept compact? */
    enum ClosureTableMode compact;
} ClosureTable;


/* Free list marker to indicate end of list */
#define FREELIST_IX_NULL (-1)

/* Free list marker to indicate the corresponding entry is used (so not free).
 * This is only used in DEBUG mode.
 */
#define FREELIST_IX_USED (-2)


/* Initialise the given table. The caller is responsible for allocating the
 * struct itself.
 *
 * It is initialised with a capacity of zero. Use enlargeClosureTable to
 * increase the capacity.
 *
 * The compact/non-compact mode of the table must be declared. This mode is
 * fixed for the lifetime of the table.
 */
void initClosureTable(ClosureTable *t, enum ClosureTableMode mode);


/* Mark the GC roots within the table. This must be called in the GC mark phase.
 */
void markClosureTable(evac_fn evac, void *user, ClosureTable *t);


/* Is the table full? Test this before inserting into it. If it is full, use
 * enlargeClosureTable to increase the capacity.
 */
INLINE_HEADER
bool isFullClosureTable(ClosureTable *t);


/* Is the table empty?
 */
INLINE_HEADER
bool isEmptyClosureTable(ClosureTable *t);


/* The current number of elements in the table.
 */
INLINE_HEADER
int sizeClosureTable(ClosureTable *t);


/* The current table capacity.
 */
INLINE_HEADER
int capacityClosureTable(ClosureTable *t);


/* Lookup the entry at an index.
 *
 * Precondition: an entry must actually exist at the given index.
 */
INLINE_HEADER
void *indexClosureTable(ClosureTable *t, int ix);


/* Increase the capacity of the table to a new given size. The existing
 * table elements (and indices) are preserved.
 *
 * Precondition: the new capacity must be (strictly) bigger than the old.
 * Precondition: for compact tables, the table must be full before enlarging.
 *
 * It is recommended to double the capacity (or some other multiplicative
 * factor), otherwise one cannot obtain the amortised O(1) insert. Note that
 * the initial capacity is zero, so you need to handle that special case when
 * doubling.
 *
 * If you have any auxiliary arrays keyed off the same table indices, then
 * increase their size too, keeping existing elements.
 *
 * Returns true on success, false if it could not allocate enough memory.
 * A failure should be propagated up to a context in which a heap overflow
 * exception can be reported.
 */
bool enlargeClosureTable(Capability *cap, ClosureTable *t, int capacity);


/* Insert an entry into the table, returning its index.
 *
 * Precondition: the table must not be full.
 */
int insertClosureTable(Capability *cap, ClosureTable *t, void *v);


/* Remove an entry from the table (in non-compact table mode).
 *
 * Precondition: an entry must actually exist at the given index.
 * Precondition: the table must have been created in non-compact mode.
 */
void removeClosureTable(Capability *cap, ClosureTable *t, int ix);


/* Remove an entry from the table (in compact table mode).
 *
 * Precondition: an entry must actually exist at the given index.
 * Precondition: the table must have been created in compact mode.
 *
 * Preserving compactness typically has the consequence that the index of
 * another existing entry will change. Outputs are provided to report the
 * remapping of the index of an existing element that got moved as a result of
 * deleting the requested element. The output variables ix_from and ix_to give
 * the source and target index of an element that moved. Note that it is
 * possible for ix_from to be equal to ix_to. This output allows, for example,
 * the caller to apply the remapping to an auxiliary table.
 */
void removeCompactClosureTable(Capability *cap, ClosureTable *t,
                               int ix, int *ix_from, int *ix_to);



/* -----------------------------------------------------------------------------
 * Private from here on down.
 *
 * The approach we take here is to use a single array allocated on the GC heap
 * (a MutableArray# / StgMutArrPtrs) to keep track of the table values. We use
 * a separate "free list" represented as an array of the same size. The free
 * list uses integer offsets as "pointers" within the array. Using int rather
 * than pointers saves space on 64bit machines. We use int, not short because
 * for I/O tracking it's totally possible to have more than 64k I/O operations
 * in flight (e.g. waiting on > 64k TCP connections).
 *
 * Contrast this for example with the stable pointer table in rts/StablePtr.c
 * which uses a table allocated on the C heap with pointers into the GC heap.
 * Using a table on the C heap means it can represent the free list within the
 * free entries in the same array (which is of course the classic allocator
 * trick). The downside of that approach is that it means markStablePtrTable
 * has to traverse the whole table, calling evac for each one. This has to be
 * done in the phase where the GC collects roots. In the non-moving GC that
 * phase is serial and contributes to pause times. By contrast, using a single
 * pointer to a heap allocated array means we have a single GC root and the GC
 * can traverse the heap array later.
 *
 * To keep things simple, the initialisation uses a zero size. This avoids code
 * duplication between init and enlarge. It also delays a little bit of memory
 * and time cost until it is needed. Some uses will not always need a non-empty
 * table at all: I/O managers that only deal with I/O readiness only have to
 * spring into action for programs that use pipes or sockets.
 *
 * -----------------------------------------------------------------------------
 */

/* For the free list, treat the StgArrBytes payload as a C int[] array */
#define StgArrBytesAsCInts(ba) ((int *)(ba->payload))

INLINE_HEADER
bool isFullClosureTable(ClosureTable *t)
{
    /* The free head will be "null" when the table is full */
    ASSERT(t->free_hd == FREELIST_IX_NULL ? t->elems == t->capacity
                                          : t->elems <  t->capacity);
    return (t->free_hd == FREELIST_IX_NULL);
}

INLINE_HEADER
bool isEmptyClosureTable(ClosureTable *t)
{
    return (t->elems == 0);
}

INLINE_HEADER
int sizeClosureTable(ClosureTable *t)
{
    return t->elems;
}

INLINE_HEADER
int capacityClosureTable(ClosureTable *t)
{
    return (t->capacity);
}

INLINE_HEADER
void *indexClosureTable(ClosureTable *t, int ix)
{
    ASSERT(ix >= 0 && ix < t->capacity);
    ASSERT(t->arr->payload[ix] != &stg_CLOSURE_TABLE_NULL_closure);
    /* in debug, was marked as used */
    ASSERT(StgArrBytesAsCInts(t->free)[ix] == FREELIST_IX_USED);

    return t->arr->payload[ix];
}

#include "EndPrivate.h"

