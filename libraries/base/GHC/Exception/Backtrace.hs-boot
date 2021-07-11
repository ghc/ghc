{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Exception.Backtrace
    ( Backtrace
    , showBacktraces
    , collectBacktraces
    ) where

import GHC.Base

data Backtrace

showBacktraces :: [Backtrace] -> String

collectBacktraces :: IO [Backtrace]
