StgMutArrPtrs *gtc_heap_view_closurePtrs(Capability *cap, StgClosure *closure);

void gtc_heap_view_closure_ptrs_in_pap_payload(StgClosure *ptrs[], StgWord *nptrs, StgClosure *fun, StgClosure **payload, StgWord size);

StgWord gtc_heap_view_closureSize(StgClosure *closure);
