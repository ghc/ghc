{-# LANGUAGE BangPatterns #-}

module Lib where

import Data.Void

f :: Void -> ()
f !_ = ()
