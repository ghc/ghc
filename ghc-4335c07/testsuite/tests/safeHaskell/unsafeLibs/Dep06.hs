{-# LANGUAGE Safe #-}
module Dep06 where

import GHC.Conc

bad1 = unsafeIOToSTM

