{-# LANGUAGE CPP #-}
#if MIN_TOOL_VERSION_ghc(9,7,0)
{-# LANGUAGE RecordWildCards #-}
module GHC.Exts.Stack (
     -- * Stack inspection
      decodeStack
    , stackFrameSize
                     )
where
import GHC.Exts.Heap.Closures
import GHC.Exts.Stack.Decode
import GHC.Exts.Stack.Constants
import Prelude

-- TODO: Pattern match may move to function arguments
stackFrameSize :: StackFrame -> Int
stackFrameSize =
  \c ->
    case c of
      UpdateFrame {} -> sizeStgUpdateFrame
      CatchFrame {} -> sizeStgCatchFrame
      CatchStmFrame {} -> sizeStgCatchSTMFrame
      CatchRetryFrame {} -> sizeStgCatchRetryFrame
      AtomicallyFrame {} -> sizeStgAtomicallyFrame
      RetSmall {..} -> sizeStgClosure + length stack_payload
      RetBig {..} -> sizeStgClosure + length stack_payload
      RetFun {..} -> sizeStgRetFunFrame + length retFunPayload
      -- The one additional word is a pointer to the StgBCO in the closure's payload
      RetBCO {..} -> sizeStgClosure + 1 + length bcoArgs
      -- The one additional word is a pointer to the next stack chunk
      UnderflowFrame {} -> sizeStgClosure + 1
      _ -> error "Unexpected closure type"
#else
module GHC.Exts.Stack where
#endif
