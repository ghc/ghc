module Escape where

import Filter
import Vector

copy :: Vec n a -> Vec n a
copy xs = xs

bad :: Hidden a -> Vec n a
bad p = copy p

projected :: (a -> Bool) -> Vec n a -> Vec m a
projected p xs = copy (fst (partitionVec p xs))
