{-# LANGUAGE Safe #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Dep05 where

import GHC.Arr

bad1 = unsafeArray

bad2 = fill

bad3 = done

bad4 = unsafeThawSTArray

