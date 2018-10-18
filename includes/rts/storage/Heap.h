/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2006-2017
 *
 * Introspection into GHC's heap representation
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "rts/storage/Closures.h"

StgMutArrPtrs *heap_view_closurePtrs(Capability *cap, StgClosure *closure);

void heap_view_closure_ptrs_in_pap_payload(StgClosure *ptrs[], StgWord *nptrs
                        , StgClosure *fun, StgClosure **payload, StgWord size);

StgWord heap_view_closureSize(StgClosure *closure);
