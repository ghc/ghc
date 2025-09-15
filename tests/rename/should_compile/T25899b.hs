{-# LANGUAGE ExplicitNamespaces #-}

module T25899b where

import Prelude (Semigroup(data (<>)))
import Data.Function (data (&))
import Data.Monoid (data Dual, data getDual)

x = Dual "Hello" <> Dual "World" & getDual
