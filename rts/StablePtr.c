/* -*- tab-width: 4 -*- */

/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2002
 *
 * Stable pointers
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "RtsAPI.h"

#include "Hash.h"
#include "RtsUtils.h"
#include "Trace.h"
#include "StablePtr.h"

#include <string.h>

/* Comment from ADR's implementation in old RTS:

  This files (together with @ghc/runtime/storage/PerformIO.lhc@ and a
  small change in @HpOverflow.lc@) consists of the changes in the
  runtime system required to implement "Stable Pointers". But we're
  getting a bit ahead of ourselves --- what is a stable pointer and what
  is it used for?

  When Haskell calls C, it normally just passes over primitive integers,
  floats, bools, strings, etc.  This doesn't cause any problems at all
  for garbage collection because the act of passing them makes a copy
  from the heap, stack or wherever they are onto the C-world stack.
  However, if we were to pass a heap object such as a (Haskell) @String@
  and a garbage collection occurred before we finished using it, we'd run
  into problems since the heap object might have been moved or even
  deleted.

  So, if a C call is able to cause a garbage collection or we want to
  store a pointer to a heap object between C calls, we must be careful
  when passing heap objects. Our solution is to keep a table of all
  objects we've given to the C-world and to make sure that the garbage
  collector collects these objects --- updating the table as required to
  make sure we can still find the object.


  Of course, all this rather begs the question: why would we want to
  pass a boxed value?

  One very good reason is to preserve laziness across the language
  interface. Rather than evaluating an integer or a string because it
  {\em might\/} be required by the C function, we can wait until the C
  function actually wants the value and then force an evaluation.

  Another very good reason (the motivating reason!) is that the C code
  might want to execute an object of sort $IO ()$ for the side-effects
  it will produce. For example, this is used when interfacing to an X
  widgets library to allow a direct implementation of callbacks.

  One final reason is that we may want to store composite Haskell
  values in data structures implemented in the C side. Serializing and
  deserializing these structures into unboxed form suitable for C may
  be more expensive than maintaining the extra layer of indirection of
  stable pointers.

  The @makeStablePointer :: a -> IO (StablePtr a)@ function
  converts a value into a stable pointer.  It is part of the @PrimIO@
  monad, because we want to be sure we don't allocate one twice by
  accident, and then only free one of the copies.

  \begin{verbatim}
  makeStablePtr#  :: a -> State# RealWorld -> (# RealWorld, a #)
  freeStablePtr#  :: StablePtr# a -> State# RealWorld -> State# RealWorld
  deRefStablePtr# :: StablePtr# a -> State# RealWorld ->
        (# State# RealWorld, a #)
  \end{verbatim}

  There may be additional functions on the C side to allow evaluation,
  application, etc of a stable pointer.

  Stable Pointers are exported to the outside world as indices and not
  pointers, because the stable pointer table is allowed to be
  reallocated for growth. The table is never shrunk for its space to
  be reclaimed.

  Future plans for stable ptrs include distinguishing them by the
  generation of the pointed object. See
  https://gitlab.haskell.org/ghc/ghc/issues/7670 for details.
*/

spEntry *stable_ptr_table = NULL;
static spEntry *stable_ptr_free = NULL;
static unsigned int SPT_size = 0;
#define INIT_SPT_SIZE 64

/* Each time the stable pointer table is enlarged, we temporarily retain the old
 * version to ensure dereferences are thread-safe (see Note [Enlarging the
 * stable pointer table]).  Since we double the size of the table each time, we
 * can (theoretically) enlarge it at most N times on an N-bit machine.  Thus,
 * there will never be more than N old versions of the table.
 */
#if SIZEOF_VOID_P == 4
#define MAX_N_OLD_SPTS 32
#elif SIZEOF_VOID_P == 8
#define MAX_N_OLD_SPTS 64
#else
#error unknown SIZEOF_VOID_P
#endif

static spEntry *old_SPTs[MAX_N_OLD_SPTS];
static uint32_t n_old_SPTs = 0;

#if defined(THREADED_RTS)
Mutex stable_ptr_mutex;
#endif

static void enlargeStablePtrTable(void);

/* -----------------------------------------------------------------------------
 * We must lock the StablePtr table during GC, to prevent simultaneous
 * calls to freeStablePtr().
 * -------------------------------------------------------------------------- */

void
stablePtrLock(void)
{
    initStablePtrTable();
    ACQUIRE_LOCK(&stable_ptr_mutex);
}

void
stablePtrUnlock(void)
{
    RELEASE_LOCK(&stable_ptr_mutex);
}

/* -----------------------------------------------------------------------------
 * Initialising the table
 * -------------------------------------------------------------------------- */

STATIC_INLINE void
initSpEntryFreeList(spEntry *table, uint32_t n, spEntry *free)
{
  spEntry *p;
  for (p = table + n - 1; p >= table; p--) {
      p->addr = (P_)free;
      free = p;
  }
  stable_ptr_free = table;
}

void
initStablePtrTable(void)
{
    if (SPT_size > 0) return;
    SPT_size = INIT_SPT_SIZE;
    stable_ptr_table = stgMallocBytes(SPT_size * sizeof(spEntry),
                                      "initStablePtrTable");
    initSpEntryFreeList(stable_ptr_table,INIT_SPT_SIZE,NULL);

#if defined(THREADED_RTS)
    initMutex(&stable_ptr_mutex);
#endif
}

/* -----------------------------------------------------------------------------
 * Enlarging the table
 * -------------------------------------------------------------------------- */

// Must be holding stable_ptr_mutex
static void
enlargeStablePtrTable(void)
{
    uint32_t old_SPT_size = SPT_size;
    spEntry *new_stable_ptr_table;

    // 2nd and subsequent times
    SPT_size *= 2;

    /* We temporarily retain the old version instead of freeing it; see Note
     * [Enlarging the stable pointer table].
     */
    new_stable_ptr_table =
        stgMallocBytes(SPT_size * sizeof(spEntry),
                       "enlargeStablePtrTable");
    memcpy(new_stable_ptr_table,
           stable_ptr_table,
           old_SPT_size * sizeof(spEntry));
    ASSERT(n_old_SPTs < MAX_N_OLD_SPTS);
    old_SPTs[n_old_SPTs++] = stable_ptr_table;

    /* When using the threaded RTS, the update of stable_ptr_table is assumed to
     * be atomic, so that another thread simultaneously dereferencing a stable
     * pointer will always read a valid address.
     */
    stable_ptr_table = new_stable_ptr_table;

    initSpEntryFreeList(stable_ptr_table + old_SPT_size, old_SPT_size, NULL);
}

/* Note [Enlarging the stable pointer table]
 *
 * To enlarge the stable pointer table, we allocate a new table, copy the
 * existing entries, and then store the old version of the table in old_SPTs
 * until we free it during GC.  By not immediately freeing the old version
 * (or equivalently by not growing the table using realloc()), we ensure that
 * another thread simultaneously dereferencing a stable pointer using the old
 * version can safely access the table without causing a segfault (see Trac
 * #10296).
 *
 * Note that because the stable pointer table is doubled in size each time it is
 * enlarged, the total memory needed to store the old versions is always less
 * than that required to hold the current version.
 */


/* -----------------------------------------------------------------------------
 * Freeing entries and tables
 * -------------------------------------------------------------------------- */

static void
freeOldSPTs(void)
{
    uint32_t i;

    for (i = 0; i < n_old_SPTs; i++) {
        stgFree(old_SPTs[i]);
    }
    n_old_SPTs = 0;
}

void
exitStablePtrTable(void)
{
    if (stable_ptr_table)
        stgFree(stable_ptr_table);
    stable_ptr_table = NULL;
    SPT_size = 0;

    freeOldSPTs();

#if defined(THREADED_RTS)
    closeMutex(&stable_ptr_mutex);
#endif
}

STATIC_INLINE void
freeSpEntry(spEntry *sp)
{
    sp->addr = (P_)stable_ptr_free;
    stable_ptr_free = sp;
}

void
freeStablePtrUnsafe(StgStablePtr sp)
{
    ASSERT((StgWord)sp < SPT_size);
    freeSpEntry(&stable_ptr_table[(StgWord)sp]);
}

void
freeStablePtr(StgStablePtr sp)
{
    stablePtrLock();
    freeStablePtrUnsafe(sp);
    stablePtrUnlock();
}

/* -----------------------------------------------------------------------------
 * Looking up
 * -------------------------------------------------------------------------- */

StgStablePtr
getStablePtr(StgPtr p)
{
  StgWord sp;

  stablePtrLock();
  if (!stable_ptr_free) enlargeStablePtrTable();
  sp = stable_ptr_free - stable_ptr_table;
  stable_ptr_free  = (spEntry*)(stable_ptr_free->addr);
  stable_ptr_table[sp].addr = p;
  stablePtrUnlock();
  return (StgStablePtr)(sp);
}

/* -----------------------------------------------------------------------------
 * Treat stable pointers as roots for the garbage collector.
 * -------------------------------------------------------------------------- */

#define FOR_EACH_STABLE_PTR(p, CODE)                                    \
    do {                                                                \
        spEntry *p;                                                     \
        spEntry *__end_ptr = &stable_ptr_table[SPT_size];               \
        for (p = stable_ptr_table; p < __end_ptr; p++) {                \
            /* Internal pointers are free slots. NULL is last in free */ \
            /* list. */                                                 \
            if (p->addr &&                                              \
                (p->addr < (P_)stable_ptr_table || p->addr >= (P_)__end_ptr)) \
            {                                                           \
                do { CODE } while(0);                                   \
            }                                                           \
        }                                                               \
    } while(0)

void
markStablePtrTable(evac_fn evac, void *user)
{
    /* Since no other thread can currently be dereferencing a stable pointer, it
     * is safe to free the old versions of the table.
     */
    freeOldSPTs();

    FOR_EACH_STABLE_PTR(p, evac(user, (StgClosure **)&p->addr););
}

/* -----------------------------------------------------------------------------
 * Thread the stable pointer table for compacting GC.
 *
 * Here we must call the supplied evac function for each pointer into
 * the heap from the stable tables, because the compacting
 * collector may move the object it points to.
 * -------------------------------------------------------------------------- */

void
threadStablePtrTable( evac_fn evac, void *user )
{
    FOR_EACH_STABLE_PTR(p, evac(user, (StgClosure **)&p->addr););
}
