{-# LANGUAGE TypeAbstractions #-}
module T where

import Data.Kind

type S :: (forall k. k -> Type) -> Type
type S x = Bool

one :: Maybe Bool -> Bool
one Nothing = False
one (Just @(S f) a) = a :: S f

two :: Bool -> Bool
two (x :: (S f)) = x :: S f
