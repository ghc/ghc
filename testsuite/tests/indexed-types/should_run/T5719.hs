{-# LANGUAGE ConstraintKinds, TypeFamilies, FlexibleInstances #-}
module Main where

import GHC.Prim (Constraint)

import Prelude hiding (Functor, fmap)

import           Data.Set (Set)
import qualified Data.Set as S (map, fromList)

class Functor f where
    type C f a :: Constraint
    type C f a = ()

    fmap :: (C f a, C f b) => (a -> b) -> f a -> f b

instance Functor Set where
    type C Set a = Ord a
    fmap = S.map

instance Functor [] where
    fmap = map
--    type C [] a = ()

testList = fmap (+1) [1,2,3]
testSet  = fmap (+1) (S.fromList [1,2,3])

main = do { print testList; print testSet }
