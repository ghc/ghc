{-# LANGUAGE Safe #-}
module Dep10 where

import GHC.ST

bad1 = liftST 

bad2 = unsafeInterleaveST

