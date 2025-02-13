{-# LANGUAGE CPP #-}
#if MIN_TOOL_VERSION_ghc(9,13,0)
{-# LANGUAGE RecordWildCards #-}

module GHC.Exts.Stack
  ( -- * Stack inspection
    decodeStack,
    stackFrameSize,
  )
where

import GHC.Exts.Heap.Closures
import GHC.Exts.Stack.Constants
import GHC.Exts.Stack.Decode
import Prelude

-- | Get the size of the `StackFrame` in words.
--
-- Includes header and payload. Does not follow pointers.
stackFrameSize :: StackFrame -> Int
stackFrameSize (UpdateFrame {}) = sizeStgUpdateFrame
stackFrameSize (CatchFrame {}) = sizeStgCatchFrame
stackFrameSize (CatchStmFrame {}) = sizeStgCatchSTMFrame
stackFrameSize (CatchRetryFrame {}) = sizeStgCatchRetryFrame
stackFrameSize (AtomicallyFrame {}) = sizeStgAtomicallyFrame
stackFrameSize (RetSmall {..}) = sizeStgClosure + length stack_payload
stackFrameSize (RetBig {..}) = sizeStgClosure + length stack_payload
stackFrameSize (RetFun {..}) = sizeStgRetFunFrame + length retFunPayload
-- The one additional word is a pointer to the StgBCO in the closure's payload
stackFrameSize (RetBCO {..}) = sizeStgClosure + 1 + length bcoArgs
-- The one additional word is a pointer to the next stack chunk
stackFrameSize (UnderflowFrame {}) = sizeStgClosure + 1
stackFrameSize (AnnFrame {}) = sizeStgAnnFrame
stackFrameSize _ = error "Unexpected stack frame type"

#else
module GHC.Exts.Stack where
#endif
