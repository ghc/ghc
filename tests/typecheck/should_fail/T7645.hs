{-# LANGUAGE TypeOperators, KindSignatures #-}
module T7645 where

import Data.Kind

data (+) a b = P

f :: ((+) a (a :: Type), Maybe)
f = undefined

