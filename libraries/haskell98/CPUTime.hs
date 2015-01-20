{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module CPUTime (
        getCPUTime, cpuTimePrecision
    ) where
import System.CPUTime
