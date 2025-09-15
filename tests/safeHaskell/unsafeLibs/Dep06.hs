{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE Safe #-}
module Dep06 where

import GHC.Conc

bad1 = unsafeIOToSTM

