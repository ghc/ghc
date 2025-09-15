#include <Rts.h>

 uint64_t hs_custom_closureSize(StgStablePtr const sp) {
    StgPtr const root = deRefStablePtr(sp);
    ASSERT(LOOKS_LIKE_CLOSURE_PTR(root));

    return closure_sizeW((StgClosure*)root);
}
