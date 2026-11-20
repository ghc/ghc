{-# LANGUAGE LinearTypes #-}

module HeadDuplicate where

import Filter (Hidden)
import Functions (Function)

bad :: (Hidden a %1 -> Hidden a %1 -> r) -> Function a %1 -> a -> r
bad combine f a = combine (f a) (f a)
