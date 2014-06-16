{-# LANGUAGE TypeFamilies #-}

module Overlap14 where

import Data.Proxy

type family F a b c where
  F a a Int = Int
  F b c d   = Bool

foo :: Proxy b -> F b [b] Bool
foo _ = False
