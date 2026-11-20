{-# LANGUAGE GADTs, LinearTypes #-}

module LinearDotDuplicate where

import Filter (Hidden)
import Vector (Vec)

data Packages a where
    Pair :: Hidden a %1 -> Hidden a %1 -> Packages a

packAgain :: Vec n a %1 -> Hidden a
packAgain xs = xs

bad :: Hidden a %1 -> Packages a
bad p = Pair (packAgain p) (packAgain p)
