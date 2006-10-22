{-# OPTIONS -findexed-types #-}

module ShouldCompile where

import Ind2_help(C(..))

zipT :: (C a, C b) => T a -> T b -> T (a,b)
zipT x y = mkT (unT x, unT y)

