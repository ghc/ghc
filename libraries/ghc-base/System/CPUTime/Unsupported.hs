module System.CPUTime.Unsupported
    ( getCPUTime
    , getCpuTimePrecision
    ) where

import GHC.IO.Exception

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
