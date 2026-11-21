{-# LANGUAGE CPP, NoExperimentalDependentTypes, NoImpredicativeTypes #-}
module TupleLengthReject where

import Filter (ProductSame, SameLength, filterVec, makeProduct)
import Vector (Vec)

#ifdef PRODUCT_FIELDS
wrong :: Vec n Int -> ProductSame Int
wrong xs = makeProduct (filterVec odd xs) (filterVec even xs)
#else
wrong :: Vec n Int -> SameLength Int
wrong xs = (filterVec odd xs, filterVec even xs)
#endif
