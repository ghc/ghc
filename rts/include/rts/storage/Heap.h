/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2006-2017
 *
 * Introspection into GHC's heap representation
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "rts/storage/Closures.h"

/* Returns NULL on allocation failure */
StgMutArrPtrs *heap_view_closurePtrs(Capability *cap, StgClosure *closure);

void heap_view_closure_ptrs_in_pap_payload(StgClosure *ptrs[], StgWord *nptrs
                        , StgClosure *fun, StgClosure **payload, StgWord size);

StgWord heap_view_closureSize(StgClosure *closure);

/*
 * Collect the pointers of a closure into the given array. The given array should be
 * large enough to hold all collected pointers e.g.
 * `heap_view_closureSize(closure)`. Returns the number of pointers collected.
 * The caller must ensure that `closure` is not modified (or moved by the GC)
 * for the duration of the call to `collect_pointers`.
 *
 * In principle this is
 *   StgWord collect_pointers(StgClosure *closure, StgWord size, StgClosure *ptrs[size]);
 * but we cannot write this and retain C++ compatibility.
 */
StgWord collect_pointers(StgClosure *closure, StgClosure *ptrs[]);
