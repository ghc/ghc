{-# LANGUAGE Safe #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Dep08 where

import GHC.IOArray

bad1 = unsafeReadIOArray

bad2 = unsafeWriteIOArray

