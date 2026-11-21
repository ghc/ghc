{-# LANGUAGE NoExperimentalDependentTypes, NoImpredicativeTypes #-}
module TupleLengthReject where

import Filter (SameLength, filterVec)
import Vector (Vec)

wrong :: Vec n Int -> SameLength Int
wrong xs = (filterVec odd xs, filterVec even xs)
