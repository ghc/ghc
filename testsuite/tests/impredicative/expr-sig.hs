{-# LANGUAGE ImpredicativeTypes, RankNTypes #-}

module ExprSig where

import Data.Kind

f :: [forall a. a->a] -> Int
f x = error "urk"

g1 = f undefined

-- This should be accepted (and wasn't)
g2 = f (undefined :: forall b. b)

f3 :: [forall a. a->a] -> b
f3 x = error "urk"

g3 = f3 (undefined :: forall b. b)

