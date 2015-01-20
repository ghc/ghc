{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

module System (
        ExitCode(ExitSuccess,ExitFailure),
        getArgs, getProgName, getEnv, system, exitWith, exitFailure
    ) where

import System.Exit
import System.Environment
import System.Process
