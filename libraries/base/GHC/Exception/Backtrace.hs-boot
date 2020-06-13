{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Exception.Backtrace
    ( Backtrace
    , showBacktraces
    , collectBacktrace
    ) where

import GHC.Base
import GHC.Show

data Backtrace

showBacktraces :: [Backtrace] -> String

collectBacktrace :: IO [Backtrace]
