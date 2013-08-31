{-# LANGUAGE TypeFamilies #-}

-- In an ideal world, this would work. But, GHC doesn't implement
-- a full infinite-type unifier, so it can't figure out that F b [b] Bool
-- can safely reduce to Bool.

module Overlap15 where

import Data.Proxy

type family F a b c where
  F a a a    = Int
  F b c Bool = Bool

foo :: Proxy b -> F b [b] Bool
foo _ = False
