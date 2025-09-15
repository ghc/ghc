{-# LANGUAGE RebindableSyntax #-}
module IfThenElseIx where

import Data.Ix (Ix, )
import Prelude

ifThenElse :: Bool -> a -> a -> a
ifThenElse True x _ = x
ifThenElse False _ x = x

data T = A | B deriving (Eq, Ord, Ix)
