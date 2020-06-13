{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Exception.Backtrace (Backtrace, showBacktraces) where

import GHC.Base
import GHC.Show

data Backtrace

showBacktraces :: [Backtrace] -> String
