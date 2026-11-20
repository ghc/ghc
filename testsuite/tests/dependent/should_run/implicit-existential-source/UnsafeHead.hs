{-# LANGUAGE DataKinds, GADTs #-}
module UnsafeHead where

import Filter
import Vector

safeHead :: Vec ('S n) a -> a
safeHead (Cons a _) = a

bad :: Hidden a -> a
bad p = safeHead p
