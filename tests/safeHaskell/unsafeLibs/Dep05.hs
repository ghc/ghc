{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UnboxedTuples #-}
module Dep05 where

import GHC.Arr

bad1 = unsafeArray

bad2 = fill

bad3 = done

bad4 = unsafeThawSTArray

