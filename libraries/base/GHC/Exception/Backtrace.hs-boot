{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Exception.Backtrace
    ( Backtrace
    , showBacktraces
    , collectBacktrace
    ) where

import GHC.Base

data Backtrace

showBacktraces :: [Backtrace] -> String

collectBacktrace :: IO [Backtrace]
