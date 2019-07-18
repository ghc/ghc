/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2019
 * Author: Daniel Gröber
 *
 * Heap root profiling.
 *
 * ---------------------------------------------------------------------------*/

#if defined(PROFILING)

#include <string.h>

#include "PosixSource.h"
#include "Rts.h"

#include "Profiling.h"
#include "TraverseHeap.h"
#include "ProfHeapInternal.h"
#include "Hash.h"
#include "StablePtr.h"

#include "RootProfile.h"

#if SIZEOF_VOID_P == 4
#define  MAX_ROOTS 8
#define  MAX_BINS  0x100
#else
#define  MAX_ROOTS 20
#define  MAX_BINS  0x100000 // 2^20 // makes 'sizes' array about 8 MiB
#endif

#if defined(DEBUG)
int g_rootProfileDebugLevel = 0;
static inline void debug(int level, const char *s, ...)
{
    va_list ap;

    if(level > g_rootProfileDebugLevel)
        return;

    va_start(ap, s);
    vprintf(s, ap);
    fflush(stdout);
    va_end(ap);
}

static const char *i2b(StgWord bin_idx)
{
    static char str[MAX_ROOTS+1];

    StgWord i;
    for(i=0; i < MAX_ROOTS; i++) {
        if(bin_idx & (1<<i))
            str[MAX_ROOTS-i-1] = '1';
        else
            str[MAX_ROOTS-i-1] = '0';
    }
    str[i] = '\0';

    return str;
}
#else
#define debug(...)
#endif

static struct rootProfState {
    StgWord n_roots;
    StgStablePtr roots[MAX_ROOTS];
    const char* descs[MAX_ROOTS];
    stackElement ses[MAX_ROOTS];
    StgWord sizes[MAX_BINS];
} g_rootProfState;

traverseState g_rootTraverseState;

static StgWord current_root;

STATIC_INLINE void
rootGetClsHdr(StgClosure *c, StgWord *computed, StgWord *bin_idx, StgWord *count)
{
    StgWord hdr = getTravData(c);
    *computed = (hdr>>=1) & 1;
    *bin_idx  = (hdr>>=1) & (MAX_BINS - 1);
    *count    = (hdr>>=MAX_ROOTS);
}

STATIC_INLINE void
rootSetComputed(traverseState *ts, StgClosure *c,
                StgWord count, StgWord bin_idx)
{
    StgWord hdr = 0;
    hdr |= count;   hdr <<= MAX_ROOTS;
    hdr |= bin_idx; hdr <<= 1;
    hdr |= 1;       hdr <<= 1;
    setTravData(ts, c, hdr);
}

static bool
rootVisit(StgClosure *c, const StgClosure *cp,
          const stackData data, const bool first_visit,
          stackAccum *acc, stackData *child_data)
{
    (void) cp;
    (void) data;
    (void) child_data;

    traverseState *ts = &g_rootTraverseState;
    struct rootProfState *ps = &g_rootProfState;

    StgWord computed;
    StgWord bin_idx;
    StgWord count;
    rootGetClsHdr(c, &computed, &bin_idx, &count);

    debug(2, "visit %p\n", c);

    if(first_visit) {
        StgWord sizeW = (StgWord)closure_sizeW(c) - sizeofW(StgProfHeader);
        debug(2, "  bin %s (%lu) += %lu\n",
              i2b(1ul<<current_root), ps->sizes[1ul<<current_root], sizeW);
        ps->sizes[1ul<<current_root] += sizeW;
        acc->subtree_sizeW += sizeW;
        return true;
    } else if(computed) {
        if((bin_idx & (1ul<<current_root))) {
            return false;
        } else {
            StgWord sizeW = (StgWord)closure_sizeW(c) - sizeofW(StgProfHeader);
            StgWord new_bin = (1ul<<current_root) | bin_idx;
            debug(2, "  bin %s (%lu) -= %lu\n",
                  i2b(bin_idx), ps->sizes[bin_idx], sizeW);
            debug(2, "  bin %s (%lu) += %lu\n",
                  i2b(new_bin), ps->sizes[new_bin], sizeW);
            ps->sizes[bin_idx] -= sizeW;
            ps->sizes[new_bin] += sizeW;
            acc->subtree_sizeW += sizeW;
            rootSetComputed(ts, c, count, new_bin);
            return true; // have to update the children's bin_idx too
        }
    }

    return first_visit;
}

static void
rootReturn(StgClosure *c, const stackAccum acc,
           StgClosure *c_parent, stackAccum *acc_parent)
{
    (void) c_parent;

    debug(2, "return %p\n", c);

    traverseState *ts = &g_rootTraverseState;

    StgWord computed;
    StgWord bin_idx;
    StgWord count;
    rootGetClsHdr(c, &computed, &bin_idx, &count);

    acc_parent->subtree_sizeW += acc.subtree_sizeW;

    if(computed) {
        if(bin_idx & (1ul<<current_root)) {
            return; // We're doing a pass to add the current root to the bin_idx
        } else {
            barf("rootReturn: Already computed! %p for root %ld", c, current_root);
        }
    } else {
        rootSetComputed(ts, c, acc.subtree_sizeW, 1ul<<current_root);
    }
}

StgWord setRootProfPtrs(StgWord n, HsStablePtr *sps, const char** descs)
{
    struct rootProfState *ps = &g_rootProfState;
    StgWord i;

    if(n > MAX_ROOTS)
        return MAX_ROOTS;

    for (i = 0; i < ps->n_roots; i++) {
        freeStablePtr(ps->roots[i]);
        free((void*)ps->descs[i]);
    }

    for (i = 0; i < n; i++) {
        ps->roots[i] = sps[i];
        ps->descs[i] = descs[i];
    }
    ps->n_roots = i;

    return 0;
}

static char *
mkBinIdentity(Arena *arena, StgWord n_roots, StgWord bin_idx,
              const char** descs, StgWord max_combined_descr_len)
{
    /* This just allocates for the maximum possible size of the concatinated
     * root descriptions together with ", " seperators. This is quite wasteful
     * but since we can only really have a small number of them anyways what
     * does it matter. */
    const StgWord len = max_combined_descr_len + n_roots + 1;
    /*                           (intercalate "-") -^      ^-null byte */
    char *identity = arenaAlloc(arena, len);
    char *ptr = identity;

    StgWord i;
    for(i = 0; i < n_roots; i++) {
        if(bin_idx & (1ul<<i)) {
            strcpy(ptr, descs[i]);
            ptr += strlen(descs[i]);
            strcpy(ptr, "-");
            ptr += strlen("-");
            ptr[0] = '\0';
        }
    }
    if(i > 0)
        ptr[-1] = '\0';

    return identity;
}

static StgWord
sumDescSizes(StgWord n_roots, const char** descs)
{
    StgWord i;
    StgWord sum = 0;
    for(i = 0; i < n_roots; i++) {
        sum += strlen(descs[i]);
    }
    return sum;
}

/**
 * Perform root profiling.
 *
 * Note:
 *   This function should be called only immediately after major garbage
 *   collection.
 **/
void
rootProfile(Time t, Census *census)
{
    (void) t;
    traverseState *ts = &g_rootTraverseState;
    struct rootProfState *ps = &g_rootProfState;
    initializeTraverseStack(ts);

    if(ps->n_roots == 0)
        return;

    const StgWord n_roots = ps->n_roots;
    const StgWord n_bins = (1<<n_roots) - 1;

    memset(ps->sizes, 0, sizeof(ps->sizes));

    /* The following double traversal is pretty inefficient. However it's the
     * simplest way to get this working for now.
     *
     * Essentially the problem is this: Say currently flip=1, if we traverse a
     * subset of the heap now then the visited bit of all reachable closures
     * will get set to one and the unreachable ones remain at zero. After the
     * traversal the flip bit is flipped, so now it's flip=0.
     *
     * If the user changes the set of roots before the next traversal we now
     * have some nodes in our set of reachable closures with visited=1 and
     * visited=0, the ones with '0' would be interpteted as "currently valid" so
     * their heap profiling header is not reset, giving us stale data.
     *
     * We solve this here by traversing twice. This always resets the visited
     * bit back to zero in the end.
     */

    debug(2, "\n=== root profile ===\n");

    ts->return_cb = &rootReturn;
    for(StgWord i = 0; i < ps->n_roots; i++) {
        StgClosure *c = (StgClosure*)deRefStablePtr(ps->roots[i]);
        debug(2, "\npush %s, root=%lu, %p\n", ps->descs[i], i, c);
        ps->ses[i].c = c;
        ps->ses[i].accum.subtree_sizeW = 0;

        current_root = i;
        traversePushClosure(ts, c, c, &ps->ses[i], nullStackData);
        traverseWorkStack(ts, &rootVisit);
    }
    traverseInvalidateClosureData(ts);


    for(StgWord i = 0; i < ps->n_roots; i++) {
        StgClosure *c = (StgClosure*)deRefStablePtr(ps->roots[i]);
        ps->ses[i].c = c;
        traversePushClosure(ts, c, c, &ps->ses[i], nullStackData);
    }

    ts->return_cb = NULL;
    traverseWorkStack(ts, NULL);
    traverseInvalidateClosureData(ts);

    debug(2, "\n\n\n=== result ===\n");

    for(StgWord i = 0; i < n_roots; i++) {
        debug(1, "root %s = %lu\n",
              ps->descs[i], ps->ses[i].accum.subtree_sizeW);
    }

    for(StgWord i = 1; i <= n_bins; i++) {
        if(ps->sizes[i] == 0)
            continue;

        const char* identity = mkBinIdentity(
            census->arena, n_roots, i,
            ps->descs, sumDescSizes(n_roots, ps->descs));

        debug(1, "bin %s = %lu\n", identity, ps->sizes[i]);

        counter *ctr = lookupHashTable(census->hash, i);
        if(!ctr) {
            ctr = heapInsertNewCounter(census, i);
            ctr->identity = identity;
        }

        ctr->c.resid = ps->sizes[i];
    }

    closeTraverseStack(ts);
}

#endif /* PROFILING */
