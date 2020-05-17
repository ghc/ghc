module GHC.Exts.Heap.FFIClosures where

#include "Rts.h"

import Prelude
import Foreign

-- TODO use sum type for what_next, why_blocked, flags?

data TSOFields = TSOFields {
    what_next :: Word16,
    why_blocked :: Word16,
    flags :: Word32,
-- Unfortunately block_info is a union without clear discriminator.
--    block_info :: TDB,
    threadId :: Word64,
    saved_errno :: Word32,
    dirty:: Word32,
    alloc_limit :: Int64,
    tot_stack_size :: Word32
-- TODO StgTSOProfInfo prof is optionally included, but looks very interesting.
}

-- | Get non-pointer fields from @StgTSO_@ (@TSO.h@)
peekTSOFields :: Ptr a -> IO TSOFields
peekTSOFields ptr = do
    what_next' <- (#peek struct StgTSO_, what_next) ptr
    why_blocked' <- (#peek struct StgTSO_, why_blocked) ptr
    flags' <- (#peek struct StgTSO_, flags) ptr
    threadId' <- (#peek struct StgTSO_, id) ptr
    saved_errno' <- (#peek struct StgTSO_, saved_errno) ptr
    dirty' <- (#peek struct StgTSO_, dirty) ptr
    alloc_limit' <- (#peek struct StgTSO_, alloc_limit) ptr
    tot_stack_size' <- (#peek struct StgTSO_, tot_stack_size) ptr

    return TSOFields {
        what_next = what_next',
        why_blocked = why_blocked',
        flags = flags',
        threadId = threadId',
        saved_errno = saved_errno',
        dirty= dirty',
        alloc_limit = alloc_limit',
        tot_stack_size = tot_stack_size'
    }
