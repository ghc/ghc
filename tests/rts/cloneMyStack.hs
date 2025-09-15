{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

import GHC.Exts (StackSnapshot#)
import GHC.Stack.CloneStack
import Foreign
import Foreign.C.Types (CUInt)
import System.Mem

foreign import ccall "expectClosureTypes" expectClosureTypes:: StackSnapshot# -> Ptr CUInt -> Int -> IO ()

-- | Clone the stack and check that all expected closures are on it in order.
-- (The check is done by closure type.)
-- In the meanwhile enforce a garbage collection to ensure that the stack
-- snapshot is still valid afterwards (is not gc'ed while in use).
main :: IO ()
main = do
  stackSnapshot <- cloneMyStack

  performMajorGC

  let (StackSnapshot stack) = stackSnapshot
  let expectedClosureTypes = [  30 -- RET_SMALL
                              , 30 -- RET_SMALL
                              , 34 -- CATCH_FRAME
                              , 36 -- STOP_FRAME
                             ]
  withArray expectedClosureTypes (\ptr -> expectClosureTypes stack ptr (length expectedClosureTypes))
