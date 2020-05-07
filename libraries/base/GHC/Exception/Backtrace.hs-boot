{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Exception.Backtrace (Backtrace) where

import GHC.Show

data Backtrace
instance Show Backtrace
