{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
module System.CPUTime.Unsupported
    ( getCPUTime
    , getCpuTimePrecision
    ) where

import GHC.Internal.IO.Exception
import Prelude
#if __GLASGOW_HASKELL__ >= 1001
import qualified GHC.Internal.Stack.Types as Rebindable (SrcLoc(..), pushCallStack, emptyCallStack)
import qualified GHC.Internal.Types as Rebindable (unpackCString#, unpackCStringUtf8#)
#endif

getCPUTime :: IO Integer
getCPUTime =
    ioError (IOError Nothing UnsupportedOperation
                     "getCPUTime"
                     "can't get CPU time"
                     Nothing Nothing)

getCpuTimePrecision :: IO Integer
getCpuTimePrecision =
    ioError (IOError Nothing UnsupportedOperation
                     "cpuTimePrecision"
                     "can't get CPU time"
                     Nothing Nothing)
